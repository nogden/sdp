(ns multimedia.streaming.test-sdp
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [multimedia.streaming.sdp :as sdp]))

(defn try-parse
  "Attempt to parse the contents of `file` as SDP."
  [file]
  (let [result {:file (str file)}]
    (try (assoc result :passed true
                       :result (sdp/parse (slurp file)))
      (catch Exception e
        (assoc result :passed false
                      :result e)))))

(deftest parse-files-in-sdp-files-directory
  "Attempt to parse all tests in the sdp-files directory"
  []
  (let [sdp-files (.listFiles (io/file "test/sdp-files"))
        results (map try-parse sdp-files)
        pass-count (count (filter :passed results))
        test-count (count results)]
    (is (= pass-count test-count)
        "All descriptions in 'test/sdp-files' should parse successfully")))
