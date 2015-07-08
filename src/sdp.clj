;; A parser and emitter for the Session Description Protocol (SDP) as described
;; in RFC 4566.

;; SDP is intended for describing multimedia sessions for the purposes of
;; session announcement, session invitation and other forms of multimedia
;; session initiaition.
(ns sdp
  "Namespace providing the published interface of the SDP library."
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [dire.core :refer [with-handler!]])
  (:import (java.time Instant)))

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
   o=jdoe 2890844526 2890842807  IP4 IN 10.47.16.5
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

(parse test-sdp-string)

{:version 0
 :origin {:username "jdoe"
          :session-id "2890844526"
          :session-version "2890842807"
          :network-type "IN"
          :address-type "IP4"
          :unicast-address "10.47.16.5"}
 :name "SDP Seminar"
 :information* "A seminar on the session description protocol"
 :uri* "http://www.example.com/seminars/sdp.pdf"
 :email* "j.doe@example.com (Jane Doe)"
 :phone* "07932 091106"
 :connection* [{:network-type "IN"
                :address-type "IP4"
                :address "224.2.17.12"  ;; May have /2 = .12, .13 ...
                :ttl* 127}]  ;; TTL only on IP4 addresses
 :timing [{:start (ntp-time 2873397496)
           :stop (ntp-time 2873404696)}]
 :repeat {:interval 23}
 }

(defn use-raw-string-as
  "Use the raw string as the value for key."
  [key]
  (fn [sdp value] (assoc sdp key value)))

(defn add-error [e & [sdp line]]
  (update-in sdp [:errors] (fnil conj []) {:message (.getMessage e)
                                           :line line
                                           :data (ex-data e)}))

(defn parse-version
  "Extracts the version number form a v line."
  [sdp line]
  (assoc sdp :version (Integer/parseInt line)))

(with-handler! #'parse-version
  "Default erronious version strings to version 0."
  java.lang.NumberFormatException
  (fn [e & [sdp line]]
    (log/warn (str "SDP description contains invalid version: '" line
                    "', assuming version 0."))
    (assoc sdp :version 0)))

(defn validate-origin
  "Throws an ex-info if any component of origin is invalid."
  [origin]
  (let [message "Invalid origin (o=) line, "
        field-count (count (remove (comp nil? val) origin))]
    (when (< field-count 6)
      (throw (ex-info (str message "expected " (count origin)
                           " fields, received " field-count ".")
                      origin)))
    (when-not (#{"IN"} (:network-type origin))
      (log/debug "SDP description contains non-standard network-type '"
                 (:network-type origin) "' in origin line (o=)."))
    (when-not (#{"IP4" "IP6"} (:address-type origin))
      (log/debug "SDP description contains non-standard address-type '"
                 (:address-type origin) "' in origin line (o=)."))
    origin))

(defn parse-origin
  "Extracts the origin details from an o line."
  [sdp line]
  (let [[username sess-id sess-version nettype addrtype unicast-address]
        (string/split line #"\s+")
        origin {:username username
                :session-id sess-id
                :session-version sess-version
                :network-type nettype
                :address-type addrtype
                :unicast-address unicast-address}]
    (assoc sdp :origin (validate-origin origin))))

(with-handler! #'parse-origin
  "Add origin errors to the error map."
  clojure.lang.ExceptionInfo
  add-error)

(def line-parsers {:v parse-version
                   :o parse-origin
                   :s (use-raw-string-as :name)  ;; What about empty s fields?
                   :i (use-raw-string-as :information) ;; Charset in a line.
                   :u (use-raw-string-as :url)
                   :e (use-raw-string-as :email)
                   :p (use-raw-string-as :phone)
                   :c (fn [acc x] (assoc acc :connection nil))
                   :t (fn [acc x] (assoc acc :timing nil))
                   :a (fn [acc x] (assoc acc :attribute nil))
                   :m (fn [acc x] (assoc acc :media nil))})

(defn ntp-time
  "Return a java.time.Instant converted from ntp; the decimal representation of
  Network Time Protocol"
  [ntp]
  (-> ntp
      Instant/ofEpochSecond
      (.minusSeconds 2208988800)))

(defn split-line [line]
  (let [[key value] (string/split line #"=" 2)
        line-type (keyword (string/trim key))]
    [line-type value]))

(defn parse-line [sdp line]
  (let [[line-type value] (split-line line)
        parser (line-type line-parsers)]
    (if parser
      (parser sdp value)
      (do
        (log/warn "Ignoring line with unknown type '" line-type "'.")
        sdp))))

(defn parse
  "Given an SDP string, parses the description into its data structure
  representation."
  [sdp-string]
  (reduce parse-line {} (string/split-lines sdp-string)))

(defn emit
  "Emits the SDP description string for the provided media session
  data structire."
  [session]
  nil)
