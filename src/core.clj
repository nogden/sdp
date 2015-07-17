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
   c=IN IP4 224.2.17.12/127
   t=2873397496 2873404696
   r=7d 1h 0 25h
   t=2879947328 2884457638
   t=2947393048 2958437394
   a=recvonly
   m=audio 49170 RTP/AVP 0
   m=video 51372 RTP/AVP 99
   a=rtpmap:99 h263-1998/90000")

(parse test-sdp-string)

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

(defn- throw-if-nil [issue func]
  (fn [value]
    (if (nil? (func value))
      (throw (Exception. issue))
      value)))

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
       :parse-as :email}
   :p {:name :phone
       :parse-as :phone}
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
                            :parse-as :integer}]}}
   :t {:name :timing
       :parse-as {:separator #"\s+"
                  :fields [{:name :start-time
                            :parse-as :instant}
                           {:name :end-time
                            :parse-as :instant}]}}
   :r {:name :repeat
       :parse-as {:separator #"\s+"
                  :fields [{:name :repeat-interval
                            :parse-as :duration}
                           {:name :active-duration
                            :parse-as :duration}
                           {:name :offsets-from-start
                            :parse-as :duration}]}}
   :z {:name :timezone
       :parse-as {:separator #" "
                  :fields [{:name :adjustment-time
                            :parse-as :instant}
                           {:name :offset
                            :parse-as :duration}]}}
   :k {:name :encryption-keys
       :parse-as {:separator #":"
                  :fields [{:name :method
                            :parse-as :string
                            :expect #{"clear" "base64" "uri" "prompt"}}
                           {:name :payload
                            :parse-as :string}]}}
   :a {:name :attribute
       :parse-as {:separator #":"
                  :fields [{:name :attribute
                            :parse-as :string}
                           {:name :value
                            :parse-as :string}]}}
   :m {:name :media
       :parse-as {:separator #"\s+"
                  :fields [{:name :media
                            :parse-as :string
                            :expect #{"audio" "video" "text" "application" "message"}}
                           {:name :port
                            :parse-as :port}
                           {:name :protocol
                            :parse-as :string
                            :expect #{"udp" "RTP/AVP" "RTP/SAVP"}}
                           {:name :format
                            :parse-as :string}]}}})

(def parse-fns
  "The set of functions used to parse individual SDP field types."
  {:string identity
   :integer #(Integer/parseInt %)
   :numeric-string (throw-if-nil "String '%v' is not alphanumeric"
                                 #(re-matches #"[0-9|A-Z|a-z]*" %))
   :instant #(Integer/parseInt %)
   :duration identity
   :unicast-address identity
   :address identity
   :email (fn [x] (throw (Exception. "Invalid email address.")))
   :phone identity
   :port identity})

(def error-fns
  "Error handlers for parsing errors."
  {:default-to (fn [[rule line default-value e]]
                 [{:warn {:type :default-value-substitution
                          :discarded (:value line)
                          :substituted default-value
                          :parser (:parse-as rule)
                          :exception e}}
                  default-value])
   :error (fn [[rule line e]]
            [{:type :parse-failed
              :field (:name rule)
              :value (:value line)
              :parser (:parse-as rule)
              :exception e} nil])})

(defn parse-simple-field
  "Parses an atomic field using the specified parse function and returns:

  [:no parsed-structure]    If parsing is successful.

  [{:warn adjustments}      If parsing is successful, but adjustments had to be
   parsed-structure]        made to recover from minor errors.

  [error-details nil]       If parsing failed."
  [{parser-key :parse-as} line relaxed]
  [:no ((parser-key parse-fns) (:value line))])

(with-handler! #'parse-simple-field
  "Handle parse errors using the error handlers defined in error-fns."
  Exception
  (fn [e & [{error-rule :on-fail :as rule} line relaxed]]
    (cond
     (nil? relaxed)       ((:error error-fns) [rule line e])
     (vector? error-rule) (let [[handler value] error-rule]
                            ((handler error-fns) [rule line value e]))
     :else                ((:error error-fns) [rule line e]))))

(defn parse-compound-field
  "Parses a compound field described by rule and returns:

  [:no parsed-structure]    If parsing is successful.

  [{:warn adjustments}      If parsing is successful, but adjustments had to be
   parsed-structure]        made to recover from minor errors.

  [error-details nil]       If parsing failed."
  [rule line relaxed]
  [:no :compound-field-parsing-not-implemented])

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

  :relaxed        Causes the reduction to continue despite errors. Error
                  information is still added to the output."
  [& flags]
  (fn [xf]
    (let [allowed-lines (volatile! #{:v})
          {:keys [relaxed]} (set flags)]
      (fn ([] (xf))
        ([result] (xf result))
        ([result {:keys [type number section] :as line}]
         (let [allowed @allowed-lines
               error (when-not (allowed type) {:type :illegal-line-type
                                               :expected allowed
                                               :received type})
               possibilities (get-in structure [section type])]
           (when possibilities
             (vreset! allowed-lines possibilities))
           (cond
            (nil? error) (xf result line)
            relaxed      (xf result (assoc line :error error))
            :else        (reduced (xf result (assoc line :error error))))))))))

(defn parse-lines
  "Returns a transducer that, given a map representation of an SDP line, will
  parse the line value and return its data structure equiverlent. The following
  flags are supported:

  :relaxed        TODO"
  [& flags]
  (fn [xf]
    (let [{:keys [relaxed]} (set flags)]
      (fn ([] (xf))
          ([result] (xf result))
          ([result {:keys [type] :as line}]
           (let [{name :name field-type :parse-as :as rule} (type parse-rules)
                 [error field]
                 (cond
                  (map? field-type)    (parse-compound-field rule line relaxed)
                  (keyword field-type) (parse-simple-field rule line relaxed)
                  :else                [:existing nil])]
             (cond
              (= :no error)       (xf result (assoc line :parsed {name field}))
              (= :existing error) (xf result line)
              (contains? error :warn) (xf result (assoc line
                                                   :parsed {name field}
                                                   :warning (:warn error)))
              relaxed  (xf result (assoc line :error error))
              :else    (reduced (xf result (assoc line :error error))))))))))

(defn parse
  "Given an SDP string, parses the description into its data structure
  representation. The following flags are supported:

  :relaxed        Attempts to continue parsing an invalid SDP description,
                  skipping erronious lines and providing default values where
                  required."
  [sdp-string & flags]
  (let [{:keys [relaxed]} (set flags)
        parser (comp (remove string/blank?)
                     (map mapify-lines)
                     add-line-numbers
                     add-sections
                     (check-line-order relaxed)
                     (parse-lines relaxed))]
    (into [] parser (string/split-lines sdp-string))))

(defn emit
  "Emits the SDP description string for the provided media session
  data structure."
  [session]
  nil)
