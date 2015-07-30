(ns multimedia.streaming.sdp.parser
  "The `multimedia.streaming.sdp.parser` namespace is an internal namespace
  providing the implementation of the parser. It is __not__ part of the
  published interface and may change without warning between versions."
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]
            [clojure.tools.logging :as log]
            [dire.core :refer [with-handler!]]))

;; # Parser Implementation

;; The parser implementation is intended to be as declarative as possible. It
;; consists of three main parts:
;;
;; - The input preparation pipeline,
;; - the parser configuration, and
;; - the execution core.
;;
;; When data is fed to the parser it is first processed by the input
;; preparation pipeline. This performs some simple transformations on the input
;; to make the parsers job easier. The prepared input is then fed to the parser
;; which parses the various fields, using a set of overridable parsing
;; functions and inserts them into the parsed structure according to the rules
;; in the parser configuration.

;; ## Input Preparation

;; The input preperation pipeline is implemented as the following set of
;; composed transducers.

(defn mapify-lines
  "`mapify-lines` transforms a textual SDP `line` into a map of its component
  parts."
  [line]
  (let [[k v] (string/split line #"=" 2)]
    {:type (keyword (string/trim k)) :value (string/triml v)}))

(defn add-line-numbers
  "`add-line-numbers` is a stateful transducer that adds a line number to each
  line. This allows the parser to produce much better error messages."
  [xf]
  (let [line-number (volatile! 0)]
    (fn ([] (xf))
        ([result] (xf result))
        ([result input]
         (xf result (assoc input :line-number (vswap! line-number inc)))))))

(defn add-sections
  "`add-sections` is a stateful transducer that adds a section identifier to
  each line, indicating to the parser whether it is currently parsing a
  session-level line, or a media-level line."
  [xf]
  (let [section (volatile! :session)]
    (fn ([] (xf))
        ([result] (xf result))
        ([result {line-type :type :as input}]
         (when (= :m line-type)
           (vreset! section :media))
         (xf result (assoc input :section @section))))))

(def line-order
  "SDP has a strict line order. The `line-order` structure specifies, for each
  line type, which line types may follow."
  {:session
   {:v #{:o}
    :o #{:s}
    :s #{:i :u :e :p :c :b :t :z :k :a :m}
    :i #{:u :e :p :c :b :t :z :k :a :m}
    :u #{:e :p :c :b :t :z :k :a :m}
    :e #{:e :p :c :b :t :z :k :a :m}
    :p #{:p :c :b :t :z :k :a :m}
    :c #{:b :t :z :k :a :m}
    :b #{:t :z :k :a :m}
    :t #{:t :r :z :k :a :m}
    :r #{:t :z :k :a :m}
    :z #{:k :a :m}
    :k #{:a :m}
    :a #{:a :m}}
   :media
   {:m #{:m :i :c :b :k :a}
    :i #{:m :c :b :k :a}
    :c #{:c :m :b :k :a}
    :b #{:m :k :a}
    :k #{:m :a}
    :a #{:m :a}}})


(defn check-line-order
  "`check-line-order` returns a transducer that verifies that the SDP lines are
  in the expected order as defined in the `line-order` structure. If the lines
  are not in the expected order an exception is thrown. The following flags are
  supported.

  `:relaxed` causes bad ordering to be treated as a non-fatal error that simply
  generates a warning."
  [& flags]
  (fn [xf]
    (let [{:keys [relaxed]} (set flags)
          allowed-lines (volatile! #{:v})]
      (fn ([] (xf))
        ([result] (xf result))
        ([result {:keys [type line-number section] :as line}]
         (let [allowed @allowed-lines
               possibilities (get-in line-order [section type])]
           (when possibilities
             (vreset! allowed-lines possibilities))
           (cond
            (allowed type) (xf result line)
            relaxed (do (log/warn "Skipping illegal line type '" type
                                  "' on line " line-number ", expected one of "
                                  allowed) result)
            :else   (throw
                     (ex-info (str "Illegal line type '" type "' on line "
                                   line-number ", expected one of " allowed)
                              {:error-type :illegal-line-type
                               :expected allowed
                               :received type
                               :line-number line-number})))))))))

;; The input data leaves the input preparation stage as a sequence of maps that
;; contain all the information needed for the parsing stage. Additionally, if
;; the parser is in its default strict mode, the lines are guaranteed to be in
;; the correct order.

;; ## Parser Configuration

;; The behaviour of the parser is dictated by the parser configuration. This
;; configuration describes the structure of the SDP protocol, how each field
;; should be parsed and how it should be inserted into the parsed SDP
;; description.

;; ### Insert Functions

;; The following functions provide the options for inserting a parsed field
;; into the SDP description.

(defn vectorize
  "`vectorize` is an insert function that causes the parsed `field` to be
  inserted into the `sdp` description as a vector under `key`. Multiple fields
  of the same type, within the same section, are appended to the vector."
  [sdp key field]
  (if (vector? (key field))
    (update-in sdp [key] into (key field))
    (update-in sdp [key] (fnil conj []) (key field))))

(defn in-last
  "`in-last` returns an insert function that causes the parsed `field` to be
  inserted as the value of `key` under the last entry in `parent`, where
  `parent` is a key in the final `sdp` structure, whose value is a vector.
  For example

    (in-last :media-descriptions)

  would add `field` as the value of `key` to the most recent entry under
  `:media-descriptions`."
  [parent]
  (fn [sdp key field]
    (let [index (dec (count (parent sdp)))]
      (assoc-in sdp [parent index key] (key field)))))

(defn vectorize-in-last
  "`vectorize-in-last` returns an insert function that causes the parsed
  `field` to be inserted as a vector, which is the value of `key`, in the last
  entry in `parent`, where `parent` is a key in the final `sdp` structure.
  Multiple fields of the same type, within the same section, are appended to
  the vector. For example

    (vectorize-in-last :media-descriptions)

  would add `field` to the vector, which is the value of `key` in the most
  recent entry under `:media-descriptions`.
  "
  [parent]
  (fn [sdp key field]
    (let [index (dec (count (parent sdp)))]
      (if (vector? (key field))
        (update-in sdp [parent index key] into (key field))
        (update-in sdp [parent index key] (fnil conj []) (key field))))))

;; ### Parse Rules

(def parse-rules
  "The `parse-rules` structure defines the rules for parsing the fields of the
  SDP protocol. It also specifies how the parsed fields should be combined into
  the final structure.

  At the top level, the `parse-rules` structure maps SDP line-types to the
  appropriate parsing rule. Each parsing rule consists of the following fields.

  `:name` is required and specifies the key, in the final SDP structure, under
  which the parsed field should be inserted.

  `:parse-as` is required and specifies how the field should be parsed. Its
  value must be either

  - a keyword matching one of the parsing functions in the `parse-fns`
    structure, or
  - a parse-spec as defined below.

`:on-fail` is optional and specifies how to recover from an error when in
  relaxed parsing mode. Its value must be either

  - a keyword matching one of the error handling functions in the `error-fns`
    structure, or
  - a two element vector, where the first element is a keyword matching one of
    the error handling functions in `error-fns`, and the second element is the
    parameter to that function.

If the key is not present, errors for that field are always treated as fatal.

  `:insert` is optional and specifies how the parsed field is inserted into the
  final SDP structure. Its value must be either

  - an insert function to be used for all parsed values, or
  - a map whose keys are section identifiers and whose values are the insert
    functions to be used when parsing values from the corresponding section.

If the key is not present, the field is inserted at the top level of the SDP
  structure. In the event of multiple values for the same field, only the last
  value will be present in the SDP structure.

  __Parse Spec__<br />
  A parse-spec describes how to parse a compound field. It is a map comprising
  the following keys.

  `:separator` is required and specifies how the compound field should be split
  into its atomic parts. Its value must be a regular expression suitable for
  consumption by `clojure.string/split`.

  `:fields` is required and specifies the list of fields that make up the
  compound field. Its value must be a vector of field-specs as defined below.

  `:repeats?` is optional and specifies whether the list of field-specs should
  be repeated until all components of the compound field have been consumed. If
  the key is present, its value is truthy, and the line contains more
  components than exist fields in the field-spec, the parser will continue,
  assuming that the remaining components are another set of the same fields.
  This is repeated until all input is consumed. The resulting parsed values
  will be inserted into the final SDP structure as a vector of compound fields,
  one for each cycle of the field-spec list.

  __Field Spec__<br />
  A field-spec describes how to parse an atomic field from a compound field. It
  is a map comprising the following keys.

  `:name` is required and specifies the key, in the parsed compound field,
  under which the sub-field should be inserted.

  `:parse-as` is required and specifes the parse function to be used when
  parsing the field. Its value must address one of the parse functions in the
  `parse-fns` structure.

  `:expect` is optional and specifies the set of possible values that are
  recognised as valid by the specification. If specified and the parsed value
  is not one of the listed values, a debug message is emitted warning about the
  use of a non-standard value. This can greatly aid debugging in multimedia
  applications.
  "
  {:v {:name :version
       :parse-as :integer
       :on-fail [:default-to 0]}
   :o {:name :origin
       :parse-as {:separator #"\s+"
                  :fields [{:name :username
                            :parse-as :string}
                           {:name :session-id
                            :parse-as :numeric-string}
                           {:name :session-version
                            :parse-as :string}
                           {:name :network-type
                            :parse-as :string
                            :expect #{"IN"}}
                           {:name :address-type
                            :parse-as :string
                            :expect #{"IP4" "IP6"}}
                           {:name :address
                            :parse-as :host}]}}
   :s {:name :name
       :parse-as :string
       :on-fail [:default-to " "]}
   :i {:name :information
       :parse-as :string
       :insert {:media (in-last :media-descriptions)}}
   :u {:name :uri
       :parse-as :string}
   :e {:name :email
       :parse-as :email
       :insert vectorize}
   :p {:name :phone
       :parse-as :phone
       :insert vectorize}
   :c {:name :connection
       :parse-as {:separator #"\s+"
                  :fields [{:name :network-type
                            :parse-as :string
                            :expect #{"IN"}}
                           {:name :address-type
                            :parse-as :string
                            :expect #{"IP4" "IP6"}}
                           {:name :address
                            :parse-as :ip-address}]}
       :insert {:media (vectorize-in-last :media-descriptions)}}
   :b {:name :bandwidth
       :parse-as {:separator #":"
                  :fields [{:name :bandwidth-type
                            :parse-as :string
                            :expect #{"CT" "AS"}}
                           {:name :bandwidth
                            :parse-as :integer}]}
       :insert {:session vectorize
                :media (vectorize-in-last :media-descriptions)}}
   :t {:name :timing
       :parse-as {:separator #"\s+"
                  :fields [{:name :start-time
                            :parse-as :instant}
                           {:name :end-time
                            :parse-as :instant}]}
       :insert vectorize}
   :r {:name :repeat
       :parse-as {:separator #"\s+"
                  :fields [{:name :repeat-interval
                            :parse-as :duration}
                           {:name :active-duration
                            :parse-as :duration}
                           {:name :offsets-from-start
                            :parse-as :duration}]}
       :insert (vectorize-in-last :timing)}
   :z {:name :timezone
       :parse-as {:separator #"\s+"
                  :fields [{:name :adjustment-time
                            :parse-as :instant}
                           {:name :offset
                            :parse-as :duration}]
                  :repeats? true}}
   :k {:name :encryption-key
       :parse-as {:separator #":"
                  :fields [{:name :method
                            :parse-as :string
                            :expect #{"clear" "base64" "uri" "prompt"}}
                           {:name :payload
                            :parse-as :string}]}
       :insert {:media (in-last :media-descriptions)}}
   :a {:name :attributes
       :parse-as {:separator #":"
                  :fields [{:name :attribute
                            :parse-as :string}
                           {:name :value
                            :parse-as :string}]}
       :insert {:session vectorize
                :media (vectorize-in-last :media-descriptions)}}
   :m {:name :media-descriptions
       :parse-as {:separator #"\s+"
                  :fields [{:name :media-type
                            :parse-as :string
                            :expect #{"audio" "video" "text"
                                      "application" "message"}}
                           {:name :port
                            :parse-as :port}
                           {:name :protocol
                            :parse-as :string
                            :expect #{"udp" "RTP/AVP" "RTP/SAVP"}}
                           {:name :format
                            :parse-as :string}]}
       :insert vectorize}})

;; ### Parse Functions

;; The following functions comprise the default parse functions used to parse
;; atomic fields.

(defn numeric-string
  "`numeric-string` is a parse function that returns the unmodified string
  value of the `field`, but throws an exception if the value is not numeric."
  [field]
  (try (bigint field) field
    (catch NumberFormatException e
      (throw (Exception. "String is not numeric")))))

(defn integer-in-range
  "`integer-in-range` returns a parse function that parses an integer from the
  `field` and returns it, if and only if the value is in the range of `min` and
  `max` inclusive. If integer parsing fails or if the value is outside of the
  specified range, an exception is thrown."
  [min max]
  (fn [field]
    (let [value (Integer. field)]
      (if (<= min value max)
        value
        (throw (Exception. (str "Value '" field "' is outside the legal range "
                                "(min: " min ", max: " max ")")))))))

(defn ip-address
  "`ip-address` is a parse function that will parse an IP version 4 or IP
  version 6 address, an optional TTL value and an optional address count from
  the `field`. The string must be in the format

    <ip-address>[/ttl][/address-count]

  and is parsed into the structure

    {:address java.net.Inet[4|6]Address
     :ttl ttl
     :address-count address-count}

  with the presence of the `:ttl` and `:address-count` keys being conditional
  upon their presence in the input string. If parsing fails an exception is
  thrown."
  [field]
  (let [[ip ttl address-count] (string/split field #"/" 3)
        address (java.net.InetAddress/getByName ip)
        result {:address address}
        IPv4 java.net.Inet4Address
        IPv6 java.net.Inet6Address]
    (match [(class address) ttl address-count]
      [IPv4 nil nil]   (if (.isMulticastAddress address)
                         (throw (Exception. (str "Multicast IPv4 address "
                                                 "requires a TTL value")))
                         result)
      [IPv4 ttl nil]   (assoc result :ttl ((integer-in-range 0 255) ttl))
      [IPv4 ttl count] (assoc result :ttl ((integer-in-range 0 255) ttl)
                                     :address-count (Integer. count))
      [IPv6 nil nil]   result
      [IPv6 count nil] (assoc result :address-count (Integer. count))
      [IPv6 _ _]       (throw (Exception. "TTL value not allowed for IPv6")))))

(def ^:dynamic parse-fns
  "The `parse-fns` structure specifies which parse function should be used to
  parse each field type. The functions specified here can be overriden by the
  user with the `custom-parser-for!` API function."
  {:string identity
   :integer #(Integer. %)
   :numeric-string numeric-string
   :instant bigint
   :duration identity
   :host identity
   :ip-address ip-address
   :email identity
   :phone identity
   :port (integer-in-range 0 65535)})

;; ### Error Functions

(def error-fns
  "The `error-fns` structure specifies how errors should be handled for each of
  the `:on-fail` conditions in the `parse-rules` structure."
  {:default-to (fn [[rule value default line-num e]]
                 (log/info "Bad value '" value "' for " (name (:name rule))
                           " on line " line-num
                           ", substituting default value '" default "'")
                 {(:name rule) default})
   :error (fn [[rule value line-num e]]
            (throw (ex-info (str "Bad value '" value "' for "
                                 (name (:name rule)) " on line " line-num)
                    {:error-type :parse-failed
                     :field (:name rule)
                     :value value
                     :parser (:parse-as rule)} e)))})

;; ## Execution Core

;; The execution core has no intrinsic understanding of the SDP format. It is
;; simply a generic parsing engine that is given a set of parse rules as
;; described in the `parse-rules` structure and a pre-processed sequence of
;; input lines in the following format.
;;
;;     {:type :v
;;      :value "0"
;;      :line-number 1
;;      :section :session}
;;
;; The lines are parsed in order and the resulting values inserted into the
;; SDP structure.
;;
;; The following functions comprise the execution core.

(defn flatten-fields
  "`flatten-fields` flattens any values in `map`, that are themselves maps,
  into `map`. If `map` and one of its values contain the same key, the child
  value takes presidence."
  [map]
  (reduce (fn [r [k v]] (if (map? v) (merge r v) (assoc r k v))) {} map))

(defn parse-simple-field
  "`parse-simple-field` parses an atomic field from `value` using the specified
  `rule`, allowing any exceptions to propogate."
  [{:keys [parse-as name expect] :as rule} value line-num relaxed]
  (let [parsed ((parse-as parse-fns) value)]
    (when-not (or (nil? expect) (expect parsed))
        (log/debug "Non-standard value '" parsed "' for '" name "' on line "
                   line-num ", expected one of " expect))
    {name parsed}))

;; This `with-handler!` call wraps the `parse-simple-field` function in an
;; exception handler that deligates any errors to the appropriate error
;; function in the `error-fns` structure.
(with-handler! #'parse-simple-field
  Exception
  (fn [e & [{error-rule :on-fail :as rule} value line-num relaxed]]
    (cond
     (nil? relaxed)       ((:error error-fns) [rule value line-num e])
     (vector? error-rule) (let [[func param] error-rule]
                            ((func error-fns) [rule value param line-num e]))
     :else                ((:error error-fns) [rule value line-num e]))))

(defn parse-compound-field
  "`parse-compound-field` parses a compound field from `value` using the rule
  (pased as the first parameter) and returns the parsed value."
  [{{:keys [repeats? separator] field-rules :fields} :parse-as name :name}
   value line-num relaxed]
  (let [fields (string/split value separator)
        [field-rules parse-fn] (if repeats?
                                 [(cycle field-rules)
                                  (comp (map parse-simple-field)
                                        (partition-all (count field-rules))
                                        (map (partial apply merge)))]
                                 [field-rules (map parse-simple-field)])
        parsed (sequence parse-fn field-rules fields
                         (repeat line-num) (repeat relaxed))]
    (if repeats?
      {name (into [] parsed)}
      {name (transduce (map flatten-fields) merge parsed)})))

(defn parse-lines
  "`parse-lines` returns a reducing function that, given a map representation
  of an SDP `line`, will parse the `line` value and return its data structure
  equiverlent. Any errors in parsing will result in an ExceptionInfo being
  thrown. The following flags are supported.

  `:relaxed` causes minor errors to be automatically corrected where possible,
  corrections are logged at the info log level. Major errors will still throw
  an ExceptionInfo."
  [& flags]
  (let [{:keys [relaxed]} (set flags)]
    (fn [result {:keys [type value line-number section] :as line}]
      (let [{:keys [insert parse-as name] :as rule} (type parse-rules)
            parsed (if (map? parse-as)
                     (parse-compound-field rule value line-number relaxed)
                     (parse-simple-field rule value line-number relaxed))
            insert (if (map? insert) (section insert) insert)
            insert (if (fn? insert)
                        insert
                       (fn [final key parsed] (conj final parsed)))]
        (insert result name parsed)))))
