;; Project configuration for Session Description Protocol library.
(set-env!
 :resource-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/tools.logging "0.3.1"]
                 [dire "0.5.3"]
                 ;; Dev dependencies
                 [it.frbracch/boot-marginalia "LATEST" :scope "test"]])

(task-options!
 pom
 {:project 'multimedia.streaming/sdp
  :version "0.1.0-SNAPSHOT"
  :description "A parser and emitter for the Session Description Protocol (SDP)
                as described by RFC 4566."
  :url "http://nogden.github.io/sdp/"
  :scm {:url "https://github.com/nogden/sdp"}
  :license {"Eclipse Public License"
            "https://www.eclipse.org/legal/epl-v10.html"}}
 repl
 {:init-ns 'multimedia.streaming.sdp}
 push
 {:repo "clojars-classic"
  :ensure-snapshot true})

(deftask build
  "Build the project jar file"
  []
  (comp (pom) (jar)))

(deftask doc
  "Generate the project documentation"
  []
  (require 'it.frbracch.boot-marginalia)
  (let [marginalia (resolve 'it.frbracch.boot-marginalia/marginalia)]
    (marginalia :file "sdp.html")))
