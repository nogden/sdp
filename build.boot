;; Project configuration for Session Description Protocol library.
(set-env!
 :source-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/tools.logging "0.3.1"]
                 [dire "0.5.3"]
                 ;; Dev dependancies
                 [it.frbracch/boot-marginalia "0.1.2" :scope "test"]])

(require '[it.frbracch.boot-marginalia :refer [marginalia pom-option]])

(task-options!
 pom
 {:project 'multimedia.streaming/sdp
  :version "0.1.0"
  :description "A parser and emitter for the Session Description Protocol (SDP)
                as described in RFC 4566."}
 repl
 {:init-ns 'multimedia.streaming.sdp}
 marginalia
 {:file "sdp.html"})
