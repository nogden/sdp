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
   t=2873397496 2873404696
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
                 (assoc sdp line-name value))
   :debug (fn [sdp line-name value arg]
            (log/debug (string/replace arg "%v" value)) sdp)
   :warn (fn [sdp line-name value arg]
           (log/warn (string/replace arg "%v" value)) sdp)
   :error (fn [sdp line-name value arg]
            (let [error-string (string/replace arg "%v" value)]
              (log/error error-string)
              (update-in sdp [:errors] (fnil conj []) error-string)))})

;; (parse test-sdp-string)
(def parsing-rules
  "The rules used to parse SDP fields."
  {"v" [:version
        :integer
        [:on-fail [:default-to 0
                   :warn "Invalid version '%v', assuming default of '0'."]]
        :required]
   "o" [:origin
        {:separator #"\s+"
         :fields [:username        [:string #{}]
                  :session-id      [:numeric-string #{}
                                    :warn "Session id '%v' is not numeric"]
                  :session-version [:string]
                  :network-type    [:string #{"IN"}
                                    :debug "Non-standard network-type '%v'"]
                  :address-type    [:string #{"IP4" "IP6"}
                                    :debug "Non-standard address-type '%v'"]
                  :unicast-address [:string]]}
        [:on-fail [:error "Invalid origin line (o=)."]]
        :required]
   "s" [:name
        :string
        [:on-fail [:default-to " "
                   :warn "Invalid empty session name, assuming default of '%d'."]]
        :required]
   "i" [:information :string]
   "u" [:url :string]
   "e" [:email :string :collate]
   "p" [:phone :string :collate]
   "c" [:connection
        {:separator #"\s+"
         :fields [:network-type       [:string #{"IN"}
                                       :debug "Non-standard network-type '%v'"]
                  :address-type       [:string #{"IP4" "IP6"}
                                       :debug "Non-standard address-type '%v'"]
                  :connection-address [:connection-address  ;; Must pass full field.
                                       :error "Invalid connection-address '%v'"]]}
        [:on-fail [:error "Invalid connection line (c=)."]]]
   "b" [:bandwidth
        {:separator #":"
         :fields [:bandwidth-type [:string #{"CT" "AS"}
                                   :debug "Non-standard bandwidth-type '%v'"]
                  :bandwidth [:integer
                              :error "Bandwidth value '%v' is not an integer"]]}
        [:on-fail [:warn "Invalid bandwidth line (b=)."]]]
   "t" [:timing
        {:separator #"\s+"
         :fields [:start-time [:instant
                               :error "Start time '%v' is not a time"]
                  :end-time   [:instant
                               :error "End time '%v' is not a time"]]}
        [:on-fail [:warn "Invalid timing line (t=)."]]
        :collate]
   "r" [:repeat
        {:separator #"\s+"
         :fields [:repeat-interval [:duration
                                    :error "Repeat interval '%v' is not a duration"]
                  :active-duration [:duration
                                    :error "Active duration '%v' is not a duration"]
                  :offsets-from-start [:duration
                                       :error "Invalid offsets list '%v'"
                                       :collate]]}
        [:on-fail [:warn "Invalid repeat line (r=)."]]
        :collate]})

(defn add-error [e & [sdp line]]
  (update-in sdp [:errors] (fnil conj []) {:message (.getMessage e)
                                           :line line
                                           :data (ex-data e)}))

(defn parse-fields [] nil)

(defn parse-line
  "Parse line accroding to the rule."
  [sdp line [line-name line-type & options]]
  (let [result (cond
                (map? line-type) (parse-fields line line-type)
                :else ((line-type parse-fns) line))]
    (assoc sdp line-name result)))

(with-handler! #'parse-line
  "Handle parsing errors according to the error rules."
  Exception
  (fn [e [sdp line [line-name _ [_ _ error-spec]]]]
    (letfn [(handle-errors [sdp [op param]]
             ((op error-fns) sdp line param))]
      (reduce handle-errors sdp error-spec))))

(defn parse-sdp [sdp line]
   (let [[line-type value] (string/split line #"=" 2)
         rule (parsing-rules line-type)]
     (if (some? rule)
       (parse-line sdp value rule)
       (do
         (log/debug (str "Unknown SDP line type '" line-type
                         "', skipping line '" line "'."))
         sdp))))

(defn parse
  "Given an SDP string, parses the description into its data structure
  representation."
  [sdp-string]
  (reduce parse-sdp {} (string/split-lines sdp-string)))

;; (parse test-sdp-string)

(defn emit
  "Emits the SDP description string for the provided media session
  data structure."
  [session]
  nil)
