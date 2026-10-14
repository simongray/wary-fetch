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

(defn- digits
  "The text of an integer of `n` digits, a one and then zeros."
  [n]
  (apply str "1" (repeat (dec n) "0")))

(deftest reading-long-numbers
  (let [cap  (digits json/max-digits)
        over (digits (inc json/max-digits))]
    (is (= 0.1 (json/read-json (str "0." over))) "a decimal of any length")
    #?(:clj
       (do (is (= (bigint cap) (json/read-json cap))
               "a bigint up to max-digits")
           (is (= (- (bigint cap)) (json/read-json (str "-" cap)))
               "with a minus sign too")
           (is (nil? (json/read-json (str "[" over "]"))) "nil past it")
           (let [started (System/currentTimeMillis)]
             (is (nil? (json/read-json (str "[" (digits 1000000) "]"))))
             (is (< (- (System/currentTimeMillis) started) 1000)
                 "at once, not in the half minute a bigint of it would take")))
       :cljs
       (is (= [##Inf ##Inf] (json/read-json (str "[" cap "," over "]")))
           "a double, as JSON.parse reads it"))))

(deftest writing-json
  (is (= "{\"type\":[\"h-entry\"],\"dc/creator\":\"Jane \\\"J\\\"\\n\",\"n\":[1,2.5,null],\"set\":[\"x\"]}"
         (json/write-json (array-map :type ["h-entry"] :dc/creator "Jane \"J\"\n" :n [1 2.5 nil] :set #{"x"})))
      "keywords by their names with the namespace, and a set as an array")
  (is (= "\"2024-03-01T09:30:00.000Z\"" (json/write-json #inst "2024-03-01T09:30:00Z"))
      "an instant as JavaScript writes it")
  (is (= "[null,null]" (json/write-json [##NaN ##Inf])) "a number that isn't finite as null")
  (let [x {:a [1 "two" {:c nil}] :b true}]
    (is (= x (json/read-json (json/write-json x))) "read back as it was")))
