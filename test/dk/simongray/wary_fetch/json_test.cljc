(ns dk.simongray.wary-fetch.json-test
  "JSON read strictly and written, the same on both platforms."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.wary-fetch.json :as json]))

(deftest reading-json
  (is (= {:me "https://simon.example/" :scope ["create" 1 -2500.0 true nil]}
         (json/read-json " {\"me\": \"https://simon.example/\", \"scope\": [\"create\", 1, -2.5e3, true, null]} ")))
  (is (= {"a" {"b" []}} (json/read-json "{\"a\":{\"b\":[]}}" {:key-fn identity}))
      "keys by the :key-fn")
  (is (= "é😀\"\\/\b\f\n\r\t" (json/read-json "\"\\u00e9\\ud83d\\ude00\\\"\\\\\\/\\b\\f\\n\\r\\t\""))
      "the escapes of RFC 8259, a surrogate pair too")
  (is (= [0 -0.5 1.0E10] (json/read-json "[0,-0.5,1e10]")))
  (testing "nil for what isn't JSON"
    (doseq [s ["" "[1,]" "{\"a\" 1}" "{a:1}" "01" "[1] x" "'a'" "\"\u0001\"" "\"\\x\"" "nul" "-" "1." ".5" "[" "{\"a\":1,}"]]
      (is (nil? (json/read-json s)) (pr-str s)))
    (is (nil? (json/read-json nil)))
    (is (nil? (json/read-json (str (apply str (repeat 600 "[")) (apply str (repeat 600 "]")))))
        "nor what nests deeper than max-depth")))

(deftest writing-json
  (is (= "{\"type\":[\"h-entry\"],\"dc/creator\":\"Jane \\\"J\\\"\\n\",\"n\":[1,2.5,null],\"set\":[\"x\"]}"
         (json/write-json (array-map :type ["h-entry"] :dc/creator "Jane \"J\"\n" :n [1 2.5 nil] :set #{"x"})))
      "keywords by their names with the namespace, and a set as an array")
  (is (= "\"2024-03-01T09:30:00.000Z\"" (json/write-json #inst "2024-03-01T09:30:00Z"))
      "an instant as JavaScript writes it")
  (is (= "[null,null]" (json/write-json [##NaN ##Inf])) "a number that isn't finite as null")
  (let [x {:a [1 "two" {:c nil}] :b true}]
    (is (= x (json/read-json (json/write-json x))) "read back as it was")))
