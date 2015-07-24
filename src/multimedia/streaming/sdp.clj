;; # What is SDP?
;;
;; SDP is intended for describing multimedia sessions for the purposes of
;; session announcement, session invitation and other forms of multimedia
;; session initiation.

;; An SDP session description is entirely textual using the ISO 10646 character
;; set in UTF-8 encoding.  SDP field names and attribute names use only the
;; US-ASCII subset of UTF-8, but textual fields and attribute values MAY use
;; the full ISO 10646 character set. Field and attribute values that use the
;; full UTF-8 character set are never directly compared, hence there is no
;; requirement for UTF-8 normalisation.

(ns multimedia.streaming.sdp
  "The multimedia.streaming.sdp namespace provides the published API for
  parsing and emitting SDP strings."
  (:require [multimedia.streaming.sdp.parser :refer [mapify-lines
                                                     add-line-numbers
                                                     add-sections
                                                     check-line-order]]
            [clojure.string :as string]))

;; # An Example

(def example-sdp-string
  "An SDP session description consists of a number of lines of text of the
  form:

  type=value

  where _type_ MUST be exactly one case-significant character and <value> is
  structured text whose format depends on <type>.  In general, <value> is
  either a number of fields delimited by a single space character or a free
  format string, and is case-significant unless a specific field defines
  otherwise.  Whitespace MUST NOT be used on either side of the '=' sign.

  An SDP session description consists of a session-level section followed by
  zero or more media-level sections.  The session-level part starts with a
  'v=' line and continues to the first media-level section.  Each media-level
  section starts with an 'm=' line and continues to the next media-level
  section or end of the whole session description.  In general, session-level
  values are the default for all media unless overridden by an equivalent
  media-level value."

  "v=0
   o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5
   s=SDP Seminar
   i=A Seminar on the session description protocol
   u=http://www.example.com/seminars/sdp.pdf
   e=j.doe@example.com (Jane Doe)
   p=+44 (0)1445 637948
   c=IN IP4 224.2.17.12/127
   b=CT:128
   t=2873397496 2873404696
   r=7d 1h 0 25h
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

;; # Mime-Type

(def mime-type
  "The mime-type string to be used for SDP data is 'application/sdp'."
  "application/sdp")

;; # Parsing

(defn parse
  "The `parse` function parses the provided `sdp-string` and returns its data
  structure representation.

    (parse example-sdp-string)

  The following `flags` can optionally be supplied to
  change the parsing behaviour.

  `:relaxed` attempts to continue parsing an invalid SDP description, skipping
  erronious lines and providing default values where required.

    (parse example-sdp-string :relaxed)"
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

;; # SDP Description Structure

(def example-sdp-description
  {:version 0
   :origin {:username "jdoe"
            :session-id "2890844526"
            :session-version "2890842807"
            :network-type "IN"
            :address-type "IP4"
            :address "10.47.16.5"}
   :name "SDP Seminar"
   :information "A Seminar on the session description protocol"
   :uri "http://www.example.com/seminars/sdp.pdf"
   :email ["j.doe@example.com (Jane Doe)"]
   :phone ["+44 (0)1445 637948"]
   :connection {:network-type "IN"
                :address-type "IP4"
                :address "224.2.17.12/127"}
   :bandwidth [{:bandwidth-type "CT"
                :bandwidth 128}]
   :timing [{:start-time 2873397496N
             :end-time 2873404696N
             :repeat [{:repeat-interval "7d"
                       :active-duration "1h"
                       :offsets-from-start "0"}]}]
   :timezone {:adjustment-time 2882844526N
              :offset "-1h"}
   :encryption-key {:method "clear"
                    :payload "gf638ebi3rh3i3o3e35767"}
   :attributes [{:attribute "recvonly"}]
   :media-descriptions [{:media-type "audio"
                         :port "49170"
                         :protocol "RTP/AVP"
                         :format "0"
                         :information "Media title"
                         :connection {:network-type "IN"
                                      :address-type "IP4"
                                      :address "224.2.17.14/127"}
                         :bandwidth [{:bandwidth-type "AT"
                                      :bandwidth 14}]
                         :attributes [{:attribute "recvonly"}
                                      {:attribute "ctlmethod"
                                       :value "serverpush"}]}
                        {:media-type "video"
                         :port "51372"
                         :protocol "RTP/AVP"
                         :format "992882844526"
                         :attributes [{:attribute "rtpmap"
                                       :value "99 h263-1998/90000"}]}]})

;; # Emitting

(defn emit
  "The `emit` function serialises the provided `sdp-description` into its
  textual form and retuns the string representation.

    (emit example-sdp-description)"
  [session]
  nil)
