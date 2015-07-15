;; A parser and emitter for the Session Description Protocol (SDP) as described
;; in RFC 4566.

;; SDP is intended for describing multimedia sessions for the purposes of
;; session announcement, session invitation and other forms of multimedia
;; session initiaition.
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
   t=2873397496 2873404696value
   r=7d 1h 0 25h
   a=recvonly
   m=audio 49170 RTP/AVP 0
   m=video 51372 RTP/AVP 99
   a=rtpmap:99 h263-1998/90000")

(defn- throw-if-nil [issue func]
  (fn [value]
    (if (nil? (func value))
      (throw (Exception. issue))
      value)))

(def parse-fns
  "The set of functions used to parse individual SDP field types."
  {:string identity
   :integer #(Integer/parseInt %)
   :numeric-string (throw-if-nil "String '%v' is not alphanumeric"
                                 #(re-matches #"[0-9|A-Z|a-z]*" %))
   :instant #(Integer/parseInt %)
   :duration identity
   :connection-address identity})

(def error-fns
  "The set of error handlers."
  {:default-to (fn [sdp line-name value arg]
                 (assoc sdp line-name arg))
   :debug (fn [sdp line-name value arg]
            (log/debug (string/replace arg "%v" value)) sdp)
   :warn (fn [sdp line-name value arg]
           (log/warn (string/replace arg "%v" value)) sdp)
   :error (fn [sdp line-name value arg]
            (let [error-string (string/replace arg "%v" value)]
              (log/error error-string)
              (update-in sdp [:errors] (fnil conj []) error-string)))})

;; (parse test-sdp-string)
(def sdp-description
  "The programatic description of the SDP format."
  {:sections
   ;; As defined in RFC 4566 page 8.
   {:session
    [["v" :version :one-only]
     ["o" :origin :one-only]
     ["s" :name :one-only]
     ["i" :information :zero-or-one]
     ["u" :uri :zero-or-one]
     ["e" :email :zero-or-more]
     ["p" :phone :zero-or-more]
     ["c" :connection :zero-or-one]
     ["b" :bandwidth :zero-or-one]
     [:section :timing :one-or-more]
     ["z" :time-zones :zero-or-one]
     ["k" :encryption-keys :zero-or-one]
     ["a" :attributes :zero-or-more]
     [:section :media :zero-or-more]]
    :timing
    [["t" :timing :one-only]
     ["r" :repeat :zero-or-one]]
    :media
    [["m" :media :one-only]
     ["i" :information :zero-or-one]
     ["c" :connection :zero-or-one]
     ["b" :bandwidth :zero-or-one]
     ["k" :encryption-keys :zero-or-one]
     ["a" :attributes :zero-or-more]]}

   :parse-rules
   {"v" {:parse-as :integer
         :on-fail [[:default-to 0]
                   [:warn "Invalid version '%v', assuming default of '0'."]]}
    "o" {:parse-as {:separator #"\s+"
                    :fields [:username        [:string]
                             :session-id      [:numeric-string #{}
                                               :warn "Session id '%v' is not numeric"]
                             :session-version [:string]
                             :network-type    [:string #{"IN"}
                                               :debug "Non-standard network-type '%v'"]
                             :address-type    [:string #{"IP4" "IP6"}
                                               :debug "Non-standard address-type '%v'"]
                             :unicast-address [:string]]}
         :on-fail [[:error "Invalid origin line (o=)."]]}
    "s" {:parse-as :string
         :on-fail [[:default-to " "]
                   [:warn "Invalid empty session name, assuming default of '%d'."]]}
    "i" {:parse-as :string}
    "u" {:parse-as :string}
    "e" {:parse-as :string}
    "p" {:parse-as :string}
    "c" {:parse-as {:separator #"\s+"
                    :fields [:network-type       [:string #{"IN"}
                                                  :debug "Non-standard network-type '%v'"]
                             :address-type       [:string #{"IP4" "IP6"}
                                                  :debug "Non-standard address-type '%v'"]
                             :connection-address [:connection-address  ;; Must pass full field.
                                                  :error "Invalid connection-address '%v'"]]}
         :on-fail [[:error "Invalid connection line (c=)."]]}
    "b" {:parse-as {:separator #":"
                    :fields [:bandwidth-type [:string #{"CT" "AS"}
                                              :debug "Non-standard bandwidth-type '%v'"]
                             :bandwidth      [:integer
                                              :error "Bandwidth value '%v' is not an integer"]]}
         :on-fail [[:warn "Invalid bandwidth line (b=)."]]}
    "z" {:parse-as {:separator #" "
                    :fields [:adjustment-time [:instant
                                               :warn "Adjustment value '%v' is not a time"]
                             :offset          [:duration
                                               :warn "Offset value '%v' is not a duration"]]}
         :on-fail [[:warn "Invalid time zone line (z=)."]]
         :options #{:repeats}}  ;; Repeat feild parsing, is this the best way to represent this.
    "k" {:parse-as {:separator #":"
                    :fields [:method [:string #{"clear" "base64" "uri" "prompt"}
                                      :debug "Non-standard encryption method '%v'"]
                             :payload [:string]]}
         :on-fail [[:warn "Invalid encryprion keys line (k=)."]]}
    "a" {:parse-as {:separator #":"
                    :fields [:attribute [:string]
                             :value [:string]]}
         :on-fail [[:warn "Invalid attribute line (a=)."]]}
    "t" {:parse-as {:separator #"\s+"
                    :fields [:start-time [:instant
                                          :error "Start time '%v' is not a time"]
                             :end-time   [:instant
                                          :error "End time '%v' is not a time"]]}
         :on-fail [[:warn "Invalid timing line (t=)."]]}
    "r" {:parse-as {:separator #"\s+"
                    :fields [:repeat-interval [:duration
                                               :error "Repeat interval '%v' is not a duration"]
                             :active-duration [:duration
                                               :error "Active duration '%v' is not a duration"]
                             :offsets-from-start [:duration
                                                  :error "Invalid offsets list '%v'"
                                                  :collate]]}
         :on-fail [[:warn "Invalid repeat line (r=)."]]}}})

(def operations
  {:match {:one-only     :parse-and-advance
           :zero-or-one  :parse-and-advance
           :zero-or-more :parse
           :one-or-more  :parse}
   :no-match {:one-only     :missing-line
              :zero-or-one  :advance
              :zero-or-more :advance
              :one-or-more  :check-previous}
   :section {:one-only     :section-and-advance
             :zero-or-one  :section-and-advance
             :zero-or-more :check
             :}})

(defn invalid [sdp [letter name _ :as rule]]
  (update-in sdp [:errors] (fnil conj [])
             {:message (str "Missing mandatory '" name "' line (" letter "=)")
              :rule rule :line :unknown}))

(defn parse-fields
  "Extract fields from line according to rule."
  [line [_ line-type & options]]
  :field-parsing-not-implemented)

(defn parse-line
  "Parse line accroding to the rule."
  [sdp line [line-name line-type {options :options} :as rule]]
  (let [result (cond
                (map? line-type) (parse-fields line rule)
                :else ((line-type parse-fns) line))]
    (assoc sdp line-name result)))

(with-handler! #'parse-line
  "Handle parsing errors according to the error rules."
  Exception
  (fn [e & [sdp line [line-name _ {error-spec :on-fail}]]]
    (letfn [(handle-errors [sdp [op param]]
             ((op error-fns) sdp line-name line param))]
      (reduce handle-errors sdp error-spec))))

(defn parse-sdp [[sdp parse-rules] [k v line-num :as line]]
  (let [[line-key line-name cardinality :as rule] (first parse-rules)
        match (condp = line-key
                k        :match
                :section k
                :no-match)]
     (if (= line-key :section)
       (do
         (parse-sdp [sdp (get-in sdp-description [:sections line-name])] line)
         :handle-sections)
       (case (get-in operations [match cardinality])
         :parse-and-advance [(parse-line sdp rule v) (drop 1 parse-rules)]
         :parse             [(parse-line sdp rule v) parse-rules]
         :advance           (recur [sdp (drop 1 parse-rules)] line)
         :missing-line      (reduced [(invalid sdp rule) parse-rules])
         ;; Could add section rules here?
         ))))

(def split-fields
  "A transducer that splits an SDP line into its line-type and value fields."
  (map #(string/split % #"=" 2)))

(def trim-line-types
  "A transducer that trims whitespace from key-value pairs."
  (map (fn [[k v]] [(string/trim k) (string/triml v)])))

(defn number-lines
  "Returns a stateful transducer that adds a line number to each line using
  op as the append function."
  [op]
  (fn [xf]
    (let [line-number (volatile! 0)]
      (fn ([] (xf))
          ([result] (xf result))
          ([result input] (xf result (op input (vswap! line-number inc))))))))

(def prep-fields (comp split-fields trim-line-types (number-lines conj)))

(sequence prep-fields (string/split-lines test-sdp-string))

(defn parse
  "Given an SDP string, parses the description into its data structure
  representation."
  [sdp-string]
  (first
   (transduce prep-fields parse-sdp
              [{} (get-in [:sections :session] sdp-description)]
              (string/split-lines sdp-string))))

(parse test-sdp-string)

(defn emit
  "Emits the SDP description string for the provided media session
  data structure."
  [session]
  nil)
