;; Project configuration for Session Description Protocol library.
(set-env!
 :source-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.7.0-RC1"]])

(task-options!
 pom
 {:project 'sdp
   :version "0.1.0"
  :description "A library for parsing and emitting Session Description Protocol."}
 repl {:init-ns 'sdp} )
