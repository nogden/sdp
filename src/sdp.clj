;; A parser and emitter for the Session Description Protocol (SDP) as
;; described in RFC 4566.

;; SDP is intended for describing multimedia sessions for the purposes
;; of session announcement, session invitation and other forms of
;; multimedia session initiaition.
(ns sdp
  "Namespace providing the published interface of the SDP library."
  (:require [clojure.string :as string]))

(def mime-type
  "The mime-type string to be used for SDP data."
  "application/sdp")

;; An SDP session description is entirely textual using the ISO 10646
;; character set in UTF-8 encoding.  SDP field names and attribute names
;; use only the US-ASCII subset of UTF-8, but textual fields and
;; attribute values MAY use the full ISO 10646 character set.  Field and
;; attribute values that use the full UTF-8 character set are never
;; directly compared, hence there is no requirement for UTF-8
;; normalisation.

;; An SDP session description consists of a number of lines of text of
;; the form:q
;;
;;    <type>=<value>
;;
;; where <type> MUST be exactly one case-significant character and
;; <value> is structured text whose format depends on <type>.  In
;; general, <value> is either a number of fields delimited by a single
;; space character or a free format string, and is case-significant
;; unless a specific field defines otherwise.  Whitespace MUST NOT be
;; used on either side of the "=" sign.

;; An SDP session description consists of a session-level section
;; followed by zero or more media-level sections.  The session-level
;; part starts with a "v=" line and continues to the first media-level
;; section.  Each media-level section starts with an "m=" line and
;; continues to the next media-level section or end of the whole session
;; description.  In general, session-level values are the default for
;; all media unless overridden by an equivalent media-level value.

(def test-sdp-string
  "v=0
   o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5
   s=SDP Seminar
   i=A Seminar on the session description protocol
   u=http://www.example.com/seminars/sdp.pdf
   e=j.doe@example.com (Jane Doe)
   c=IN IP4 224.2.17.12/127
   t=2873397496 2873404696
   a=recvonly
   m=audio 49170 RTP/AVP 0
   m=video 51372 RTP/AVP 99
   a=rtpmap:99 h263-1998/90000")

(def line-parsers {:v "v-line"
                   :o "o-line"
                   :i "i-line"
                   :u "u-line"
                   :e "e-line"
                   :c "c-line"
                   :t "t-line"
                   :a "a-line"
                   :m "m-line"})

(defn- split-line [line]
  (let [[key value] (string/split line #"=" 2)
        line-type (keyword (string/trim key))]
    [line-type value]))

(defn- )

(def sdp-parse
  "A transducer that implements an SDP parser."
  (map split-line))

(defn parse
  "Given an SDP string, parses the description into its data structure
  representation."
  [sdp-string]
  (transduce sdp-parse {} (string/split-lines sdp-string)))

(defn emit
  "Emits the SDP description string for the provided media session
  data structire."
  [session]
  nil)

(parse test-sdp-string)
