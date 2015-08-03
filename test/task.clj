(ns task
  (:require [boot.core :refer [deftask]]
            [clojure.java.io :as io]
            [multimedia.streaming.sdp :as sdp]))

(defn- try-parse
  [file]
  (let [result {:file (str file)}]
    (try (assoc result :passed true
                       :result (sdp/parse (slurp file)))
      (catch Exception e
        (assoc result :passed false
                      :result e)))))

(deftask tests
  "Run the automated tests"
  []
  (print "Running tests... ")
  (flush)
  (let [sdp-files (.listFiles (io/file "test/sdp-files"))
        results (map try-parse sdp-files)
        pass-count (count (filter :passed results))
        test-count (count results)]
    (println pass-count " of " test-count " passed\n")
    (when (< pass-count test-count)
      (doseq [failed (remove :passed results)]
        (println "FAILED: " (:file failed) "\n" (:result failed) "\n\n")))))
