(ns multimedia.streaming.sdp.parser
  "The multimedia.streaming.sdp.parser namespace is an internal namespace
  providing the utility functions used within the parser. It is __not__ part of
  the published interface and may change without warning between versions."
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [dire.core :refer [with-handler!]]))

(def line-order
  "Some lines in each description are REQUIRED and some are OPTIONAL, but all
  MUST appear in exactly the order given here."
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

(defn vectorize
  "Collate duplicate elements of this type into a vector under the same key."
  [final key element]
  (update-in final [key] (fnil conj []) (key element)))

(defn in-last
  "Inserts an element of this type under the most recent entry for parent."
  [parent]
  (fn [final key element]
    (let [index (dec (count (parent final)))]
      (assoc-in final [parent index key] (key element)))))

(defn vectorize-in-last
  "Insert elements of this type into a vector under the most recent entry for
  parent."
  [parent]
  (fn [final key element]
    (let [index (dec (count (parent final)))]
      (update-in final [parent index key] (fnil conj []) (key element)))))

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
                            :parse-as :address}]}
       :insert {:media (in-last :media-descriptions)}}
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
     (vector? error-rule) (let [[func param] error-rule]
                            ((func error-fns) [rule value param line-num e]))
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
  the output and the reduction is terminated. The following flags are
  supported:

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

(defn parse-lines
  "Returns a reducing function that, given a map representation of an SDP line,
  will parse the line value and return its data structure equiverlent. Any
  errors in parsing will result in an ExceptionInfo being thrown. The following
  flags are supported:

  :relaxed        Causes minor errors to be automatically corrected where
                  possible, corrections are logged at the info log level. Major
                  errors will still throw an ExceptionInfo."
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
