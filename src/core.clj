;; A parser and emitter for the Session Description Protocol (SDP) as described
;; in RFC 4566.

;; SDP is intended for describing multimedia sessions for the purposes of
;; session announcement, session invitation and other forms of multimedia
;; session initiation.
(ns sdp.core
  "Namespace providing the published interface of the SDP library."
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [dire.core :refer [with-handler!]]))

(def mime-type
  "The mime-type string to be used for SDP data."
  "application/sdp")

;; An SDP session description is entirely textual using the ISO 10646 character
;; set in UTF-8 encoding.  SDP field names and attribute names use only the
;; US-ASCII subset of UTF-8, but textual fields and attribute values MAY use
;; the full ISO 10646 character set. Field and attribute values that use the
;; full UTF-8 character set are never directly compared, hence there is no
;; requirement for UTF-8 normalisation.

;; An SDP session description consists of a number of lines of text of the
;; form:q
;;
;;    <type>=<value>
;;
;; where <type> MUST be exactly one case-significant character and <value> is
;; structured text whose format depends on <type>.  In general, <value> is
;; either a number of fields delimited by a single space character or a free
;; format string, and is case-significant unless a specific field defines
;; otherwise.  Whitespace MUST NOT be used on either side of the "=" sign.

;; An SDP session description consists of a session-level section followed by
;; zero or more media-level sections.  The session-level part starts with a
;; "v=" line and continues to the first media-level section.  Each media-level
;; section starts with an "m=" line and continues to the next media-level
;; section or end of the whole session description.  In general, session-level
;; values are the default for all media unless overridden by an equivalent
;; media-level value.

(def test-sdp-string
  "v=0
   o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5
   s=SDP Seminar
   i=A Seminar on the session description protocol
   u=http://www.example.com/seminars/sdp.pdf
   e=j.doe@example.com (Jane Doe)
   p=+44 (0)1226 241814
   c=IN IP4 224.2.17.12/127
   b=CT:128
   t=2873397496 2873404696
   r=7d 1h 0 25h
   t=2879947328 2884457638
   t=2947393048 2958437394
   z=2882844526 -1h 2898848070 0
   k=clear:gf638ebi3rh3i3o3e35767
   a=recvonly
   m=audio 49170 RTP/AVP 0
   i=Media title
   c=IN IP4 224.2.17.14/127
   b=AT:14
   a=recvonly
   a=ctlmethod:serverpush
   m=video 51372 RTP/AVP 992882844526
   a=rtpmap:99 h263-1998/90000")

(def target {:version 0
             :origin {:username "jdoe"
                      :session-id "2890844526"
                      :session-version 2890842807
                      :network-type "IN"
                      :address-type "IP4"
                      :address "10.47.16.5"}
             :session-name "SDP Seminar"
             :information "A Seminar on the session description protocol"
             :uri "http://www.example.com/seminars/sdp.pdf"
             :email ["j.doe@example.com (Jane Doe)"]
             :phone "+44 (0)1226 241814"
             :connection {:network-type "IN"
                          :address-type "IP4"
                          :address "224.2.17.12"
                          :ttl 127}
             :bandwidth [{:type "CT"
                          :value 128}]
             :timing [{:start 2873397496N
                       :stop 2873404696N
                       :repeat [{:repeat-interval "7d"
                                 :active-duration "1h"
                                 :offsets-from-start ["0" "25h"]}]}]
             :timezone [{:adjustment-time 2882844526N
                         :offset "-1h"}]
             :encryption-key {:method "clear"
                              :payload "gf638ebi3rh3i3o3e35767"}
             :attributes [{:attribute "recvonly"}]
             :media-descriptions [{:media-type "audio"
                                   :port 49170
                                   :protocol "RTP/AVP"
                                   :format "0"
                                   :information "Media title"
                                   :connection {:network-type "IN"
                                                :address-type "IP4"
                                                :address "224.2.17.14"
                                                :ttl 127}
                                   :bandwidth [{:type "AT"
                                                :value 14}]
                                   :attributes [{:attribute "recvonly"}
                                                {:attribute "ctlmethod"
                                                 :value "serverpush"}]}
                                  {:media-type "video"
                                   :port 51372
                                   :protocol "RTP/AVP"
                                   :format "992882844526"
                                   :attributes [{:attribute "rtpmap"
                                                 :value "99 h263-1998/90000"}]}]})

(def structure
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
    :c #{:m :b :k :a}
    :b #{:m :k :a}
    :k #{:m :a}
    :a #{:m :a}}})

(def parse-rules
  "The rules that determine how each field is parsed."
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
                            :parse-as :unicast-address}]}}
   :s {:name :name
       :parse-as :string
       :on-fail [:default-to " "]}
   :i {:name :information
       :parse-as :string}
   :u {:name :uri
       :parse-as :string}
   :e {:name :email
       :parse-as :email
       :insert :vector}
   :p {:name :phone
       :parse-as :phone
       :insert :vector}
   :c {:name :connection
       :parse-as {:separator #"\s+"
                  :fields [{:name :network-type
                            :parse-as :string
                            :expect #{"IN"}}
                           {:name :address-type
                            :parse-as :string
                            :expect #{"IP4" "IP6"}}
                           {:name :address
                            :parse-as :address}]}}
   :b {:name :bandwidth
       :parse-as {:separator #":"
                  :fields [{:name :bandwidth-type
                            :parse-as :string
                            :expect #{"CT" "AS"}}
                           {:name :bandwidth
                            :parse-as :integer}]}
       :insert :vector}
   :t {:name :timing
       :parse-as {:separator #"\s+"
                  :fields [{:name :start-time
                            :parse-as :instant}
                           {:name :end-time
                            :parse-as :instant}]}
       :insert :vector}
   :r {:name :repeat
       :parse-as {:separator #"\s+"
                  :fields [{:name :repeat-interval
                            :parse-as :duration}
                           {:name :active-duration
                            :parse-as :duration}
                           {:name :offsets-from-start
                            :parse-as :duration}]}
       :insert (fn [acc parsed]
                 (let [index (dec (count (:timing acc)))]
                   (update-in acc [:timing index :repeat]
                              (fnil conj []) parsed)))}
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
                            :parse-as :string}]}}
   :a {:name :attributes
       :parse-as {:separator #":"
                  :fields [{:name :attribute
                            :parse-as :string}
                           {:name :value
                            :parse-as :string}]}
       :insert :vector}
   :m {:name :media-descriptions
       :parse-as {:separator #"\s+"
                  :fields [{:name :media-type
                            :parse-as :string
                            :expect #{"audio" "video" "text" "application" "message"}}
                           {:name :port
                            :parse-as :port}
                           {:name :protocol
                            :parse-as :string
                            :expect #{"udp" "RTP/AVP" "RTP/SAVP"}}
                           {:name :format
                            :parse-as :string}]}
       :insert :vector}})

(defn- throw-if-nil [issue func]
  (fn [value]
    (if (nil? (func value))
      (throw (Exception. issue))
      value)))

(def parse-fns
  "The set of functions used to parse individual SDP field types."
  {:string identity
   :integer #(Integer. %)
   :numeric-string (throw-if-nil "String '%v' is not alphanumeric"
                                 #(re-matches #"[0-9|A-Z|a-z]*" %))
   :instant bigint
   :duration identity
   :unicast-address identity
   :address identity
   :email identity
   :phone identity
   :port identity})

(def error-fns
  "Error handlers for parsing errors."
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

(defn parse-simple-field
  "Parses an atomic field using the specified parse function and returns the
  parsed value."
  [{:keys [parse-as name expect]} value line-num relaxed]
  (let [parsed ((parse-as parse-fns) value)
        result {name parsed}]
    (when-not (or (nil? expect) (expect parsed))
        (log/debug "Non-standard value '" parsed "' for '" name "' on line "
                   line-num ", expected one of " expect))
    result))

(with-handler! #'parse-simple-field
  "Handle parse errors using the error handlers defined in error-fns."
  Exception
  (fn [e & [{error-rule :on-fail :as rule} value line-num relaxed]]
    (cond
     (nil? relaxed)       ((:error error-fns) [rule value line-num e])
     (vector? error-rule) (let [[handler param] error-rule]
                            ((handler error-fns) [rule value param line-num e]))
     :else                ((:error error-fns) [rule value line-num e]))))

(defn parse-compound-field
  "Parses a compound field described by rule and returns the parsed value."
  [{spec :parse-as name :name} value line-num relaxed]
  (let [fields (string/split value (:separator spec))
        field-rules (if (get-in spec [:fields :repeats?])
                      (cycle (:fields spec))
                      (:fields spec))
        parsed (map parse-simple-field field-rules fields
                    (repeat line-num) (repeat relaxed))]
    {name (apply merge parsed)}))

(defn mapify-lines
  "Splits an SDP line into a map of its component parts."
  [line]
  (let [[k v] (string/split line #"=" 2)]
    {:type (keyword (string/trim k)) :value (string/triml v)}))

(defn add-line-numbers
  "A stateful transducer that adds a line number to each line."
  [xf]
  (let [line-number (volatile! 0)]
    (fn ([] (xf))
        ([result] (xf result))
        ([result input]
         (xf result (assoc input :line-number (vswap! line-number inc)))))))

(defn add-sections
  "A stateful transducer that adds the a section identifier to each line."
  [xf]
  (let [section (volatile! :session)]
    (fn ([] (xf))
        ([result] (xf result))
        ([result {line-type :type :as input}]
         (when (= :m line-type)
           (vreset! section :media))
         (xf result (assoc input :section @section))))))

(defn check-line-order
  "Returns a transducer that ensures that the SDP lines are in the expected
  order. If the lines are not in the expected the error details are added to
  the output and the reduction is terminated. The following flags are supported:

  :relaxed        Causes bad ordering to be treated as a non-fatal error that
                  only generates a log message."
  [& flags]
  (fn [xf]
    (let [{:keys [relaxed]} (set flags)
          allowed-lines (volatile! #{:v})]
      (fn ([] (xf))
        ([result] (xf result))
        ([result {:keys [type line-number section] :as line}]
         (let [allowed @allowed-lines
               possibilities (get-in structure [section type])]
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

(defn parse-lines
  "Returns a reducing function that, given a map representation of an SDP line,
  will parse the line value and return its data structure equiverlent. Any
  errors in parsing will result in an ExceptionInfo being thrown. The following
  flags are supported:

  :relaxed        Causes minor errors to be automatically corrected where
                  possible, corrections are logged at the info log level. Major
                  errros will still throw an ExceptionInfo."
  [& flags]
  (let [{:keys [relaxed]} (set flags)]
    (fn [result {:keys [type value line-number section] :as line}]
      (let [{:keys [insert parse-as name] :as rule} (type parse-rules)
            parsed (if (map? parse-as)
                     (parse-compound-field rule value line-number relaxed)
                     (parse-simple-field rule value line-number relaxed))]
        (cond
         (= :vector insert) (update-in result [name] (fnil conj []) (name parsed))
         (fn? insert)       (insert result (name parsed))
         :else              (conj result parsed))))))

(defn parse
  "Given an SDP string, parses the description into its data structure
  representation. The following flags are supported:

  :relaxed        Attempts to continue parsing an invalid SDP description,
                  skipping erronious lines and providing default values where
                  required."
  [sdp-string & flags]
  (let [{:keys [relaxed]} (set flags)
        prep-lines (comp (remove string/blank?)
                         (map mapify-lines)
                         add-line-numbers
                         add-sections
                         (check-line-order relaxed))]
    (->> sdp-string
         string/split-lines
         (transduce prep-lines (completing (parse-lines relaxed)) {}))))

(parse test-sdp-string)

(defn emit
  "Emits the SDP description string for the provided media session
  data structure."
  [session]
  nil)
