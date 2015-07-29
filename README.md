# sdp
A [Clojure](http://clojure.org) parser and emitter for the Session Description
Protocol (SDP) as described by [RFC 4566](https://tools.ietf.org/html/rfc4566).

## Installation

Available on Clojars:

```clojure
    [multimedia.streaming/sdp]
```

## Documentation

Full API and implementation documentation is available [here](http://nogden.github.io/sdp/).

## Usage

```clojure
(ns example
  (:require [multimedia.streaming.sdp :refer :all])

;; Parse an SDP string
(def description (parse example-sdp-string))

;; Resulting structure is a simple set of nested maps
(:name description) ;; => "SDP Seminar"
(get-in description [:media-descriptions 0 :media-type]) ;; => "audio"
(get-in description [:media-descriptions 0 :port]) ;; => 49170

;; Overridable parsers allow for integration with custom types
;; E.g. To parse all ports as strings
(use-custom-parsers!
  {:port identity})

(get-in (parse example-sdp-string) [:media-descriptions 0 :port]) ;; => "49170"
```

## Still to Come

1. Optional representation of instants and durations using `java.time` or JodaTime
2. Pluggable attribute parsing
3. Emitting

## License

Copyright © 2015 Nick Ogden

Distributed under the Eclipse Public License, the same as Clojure.

