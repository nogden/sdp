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

(into [] prep-fields (string/split-lines test-sdp-string))

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

(def sdp-format
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
     ["a" :attributes :zero-or-more]]}})

(def structure
  {:session
   {:v {:on-fail [[:default-to 0]
                  [:warn "Invalid version '%v', assuming default of '0'."]]
        :allowed-next #{:o}}
    :o {:on-fail [[:error "Invalid origin line (o=)."]]
        :allowed-next #{:s}}
    :s {:on-fail [[:default-to " "]
                  [:warn "Invalid empty session name, assuming default of '%d'."]]
        :allowed-next #{:i :u :e :p :c :b :t :z :k :a :m}}
    :i {:allowed-next #{:u :e :p :c :b :t :z :k :a :m}}
    :u {:allowed-next #{:e :p :c :b :t :z :k :a :m}}
    :e {:allowed-next #{:e :p :c :b :t :z :k :a :m}}
    :p {:allowed-next #{:p :c :b :t :z :k :a :m}}
    :c {:on-fail [[:error "Invalid connection line (c=)."]]
        :allowed-next #{:b :t :z :k :a :m}}
    :b {:on-fail [[:warn "Invalid bandwidth line (b=)."]]
        :allowed-next #{:t :z :k :a :m}}
    :t {:on-fail [[:warn "Invalid timing line (t=)."]]
        :allowed-next #{:t :r :z :k :a :m}}
    :r {:on-fail [[:warn "Invalid repeat line (r=)."]]
        :allowed-next #{:t :z :k :a :m}}
    :z {:on-fail [[:warn "Invalid time zone line (z=)."]]
        :options #{:repeats}
        :allowed-next #{:k :a :m}}
    :k {:on-fail [[:warn "Invalid encryprion keys line (k=)."]]
        :allowed-next #{:a :m}}
    :a {:on-fail [[:warn "Invalid attribute line (a=)."]]
        :allowed-next #{:a :m}}}
   :media
   {:m {:allowed-next #{:m :i :c :b :k :a}}
    :i {:allowed-next #{:m :c :b :k :a}}
    :c {:allowed-next #{:m :b :k :a}}
    :b {:allowed-next #{:m :k :a}}
    :k {:allowed-next #{:m :a}}
    :a {:allowed-next #{:m :a}}}})

(defn- throw-if-nil [issue func]
  (fn [value]
    (if (nil? (func value))
      (throw (Exception. issue))
      value)))

(def parse-rules
  "The rules that determine how each field is parsed."
  {:v :integer
   :o {:separator #"\s+"
       :fields [:username        [:string]
                :session-id      [:numeric-string #{}
                                  :warn "Session id '%v' is not numeric"]
                :session-version [:string]
                :network-type    [:string #{"IN"}
                                  :debug "Non-standard network-type '%v'"]
                :address-type    [:string #{"IP4" "IP6"}
                                  :debug "Non-standard address-type '%v'"]
                :unicast-address [:string]]}
   :s :string
   :i :string
   :u :string
   :e :string
   :p :string
   :c {:separator #"\s+"
       :fields [:network-type       [:string #{"IN"}
                                     :debug "Non-standard network-type '%v'"]
                :address-type       [:string #{"IP4" "IP6"}
                                     :debug "Non-standard address-type '%v'"]
                :connection-address [:connection-address  ;; Must pass full field.
                                     :error "Invalid connection-address '%v'"]]}
   :b {:separator #":"
       :fields [:bandwidth-type [:string #{"CT" "AS"}
                                 :debug "Non-standard bandwidth-type '%v'"]
                :bandwidth      [:integer
                                 :error "Bandwidth value '%v' is not an integer"]]}
   :t {:separator #"\s+"
       :fields [:start-time [:instant
                             :error "Start time '%v' is not a time"]
                :end-time   [:instant
                             :error "End time '%v' is not a time"]]}
   :r {:separator #"\s+"
       :fields [:repeat-interval [:duration
                                  :error "Repeat interval '%v' is not a duration"]
                :active-duration [:duration
                                  :error "Active duration '%v' is not a duration"]
                :offsets-from-start [:duration
                                     :error "Invalid offsets list '%v'"
                                     :collate]]}
   :z {:separator #" "
       :fields [:adjustment-time [:instant
                                  :warn "Adjustment value '%v' is not a time"]
                :offset          [:duration
                                  :warn "Offset value '%v' is not a duration"]]}
   :k {:separator #":"
       :fields [:method [:string #{"clear" "base64" "uri" "prompt"}
                         :debug "Non-standard encryption method '%v'"]
                :payload [:string]]}
   :a {:separator #":"
       :fields [:attribute [:string]
                :value [:string]]}
   :m {:separator #"\s+"
       :fields [:media [:string #{"audio" "video" "text" "application" "message"}]
                :port [:port]
                :protocol [:string #{"udp" "RTP/AVP" "RTP/SAVP"}]
                :format [:string]]}})

(def parse-fns
  "The set of functions used to parse individual SDP field types."
  {:string identity
   :integer #(Integer/parseInt %)
   :numeric-string (throw-if-nil "String '%v' is not alphanumeric"
                                 #(re-matches #"[0-9|A-Z|a-z]*" %))
   :instant #(Integer/parseInt %)
   :duration identity
   :connection-address identity})

(defn mapify
  "Splits an SDP line into a map of its component parts"
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
  the output and, if the :strict flag is passed, the reduction is terminated."
  [& flags]
  (fn [xf]
    (let [allowed-lines (volatile! #{:v})
          {:keys [strict]} (set flags)]
      (fn ([] (xf))
        ([result] (xf result))
        ([result {:keys [type number section] :as line}]
         (let [allowed @allowed-lines
               error (when-not (allowed type) {:type :illegal-line-type
                                               :expected allowed
                                               :received type})]
           (vreset! allowed-lines (get-in structure [section type :allowed-next]))
           (cond
            (nil? error) (xf result line)
            strict (reduced (xf result (assoc line :error error)))
            :else (xf result (assoc line :error error)))))))))

(def prep-fields (comp (remove string/blank?)
                       (map mapify)
                       add-line-numbers
                       add-sections
                       (check-line-order :strict)))

(into [] prep-fields (string/split-lines test-sdp-string))

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
