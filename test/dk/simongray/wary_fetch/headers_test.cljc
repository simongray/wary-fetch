(ns dk.simongray.wary-fetch.headers-test
  "The values of headers: content ranges and the If-Range that resumes a
  file, links, and when to ask again by Retry-After, Cache-Control and
  Expires."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.wary-fetch.headers :as headers]))

(deftest content-ranges
  (is (= {:start 0 :end 1023 :total 146515} (headers/content-range "bytes 0-1023/146515")))
  (is (= {:start 5 :end 9 :total nil} (headers/content-range "bytes 5-9/*")))
  (is (nil? (headers/content-range "bytes */1000")) "an unsatisfiable range names none"))

(deftest the-size-of-a-whole-file
  (is (= 146515 (headers/complete-length "bytes 0-1023/146515")))
  (is (= 1000 (headers/complete-length "Bytes */1000")) "the form of a 416, in any case")
  (doseq [s [nil "" "bytes 5-9/*" "bytes */*" "bytes */-1"]]
    (is (nil? (headers/complete-length s)) (pr-str s))))

(deftest the-tags-of-if-none-match
  (is (= "\"v2\"" (headers/if-none-match "\"v2\"")) "a strong entity tag as it is")
  (is (= "\"v2\", W/\"v2\"" (headers/if-none-match "W/\"v2\""))
      "a weak one after its strong form, which a CDN that weakened it matches"))

(deftest a-validator-for-if-range
  (let [modified "Tue, 13 Oct 2026 12:00:00 GMT"]
    (is (= "\"v2\"" (headers/if-range {"etag" ["\"v2\""] "last-modified" modified}))
        "a strong entity tag")
    (is (nil? (headers/if-range {"etag" "W/\"v2\"" "last-modified" modified
                                 "date" "Tue, 13 Oct 2026 13:00:00 GMT"}))
        "not a weak one, nor the date when there's an entity tag")
    (is (= modified (headers/if-range {"last-modified" modified
                                       "date"          "Tue, 13 Oct 2026 12:00:01 GMT"}))
        "a date a second or more before the Date of its answer")
    (is (nil? (headers/if-range {"last-modified" modified "date" modified}))
        "not a date that the file can still change within")
    (is (nil? (headers/if-range {"last-modified" modified})) "nor one without a Date")
    (is (nil? (headers/if-range {})))))

(deftest when-to-ask-again
  (let [at #inst "2026-09-27T12:00:00Z"]
    (testing "Retry-After, in seconds or as a date"
      (is (= #inst "2026-09-27T12:02:00Z" (headers/not-before {"retry-after" ["120"]} at)))
      (is (= #inst "2026-09-27T12:40:00Z" (headers/not-before {"retry-after" "Sun, 27 Sep 2026 12:40:00 GMT"} at))))
    (testing "the three forms of a date of HTTP"
      (doseq [date ["Sun, 27 Sep 2026 12:40:00 GMT" "Sunday, 27-Sep-26 12:40:00 GMT" "Sun Sep 27 12:40:00 2026"]]
        (is (= #inst "2026-09-27T12:40:00Z" (headers/not-before {"retry-after" [date]} at)) date))
      (is (= #inst "2026-09-06T12:40:00Z" (headers/not-before {"retry-after" ["Sun Sep  6 12:40:00 2026"]} #inst "2026-09-06T12:00:00Z"))
          "a day of one digit has a space before it"))
    (testing "the freshness of Cache-Control, less the age, else Expires"
      (is (= #inst "2026-09-27T12:08:20Z" (headers/not-before {"cache-control" ["public, max-age=600"] "age" ["100"]} at)))
      (is (= #inst "2026-09-27T12:30:00Z" (headers/not-before {"expires" ["Sun, 27 Sep 2026 12:30:00 GMT"]} at))))
    (testing "a date counts from the Date of the response, whatever the clock or the zone of the server"
      (let [two-hours-ahead {"date" ["Sun, 27 Sep 2026 14:00:00 GMT"]}]
        (is (= #inst "2026-09-27T12:30:00Z" (headers/not-before (assoc two-hours-ahead "expires" ["Sun, 27 Sep 2026 14:30:00 GMT"]) at)))
        (is (= #inst "2026-09-27T12:20:00Z" (headers/not-before (assoc two-hours-ahead "retry-after" ["Sun, 27 Sep 2026 14:20:00 GMT"]) at)))
        (is (= #inst "2026-09-27T12:20:00Z"
               (headers/not-before (assoc two-hours-ahead "expires" ["Sun, 27 Sep 2026 14:30:00 GMT"] "age" ["600"]) at))
            "less the age")))
    (testing "a Retry-After that does not read counts as none"
      (is (= #inst "2026-09-27T12:10:00Z" (headers/not-before {"retry-after" ["soon"] "cache-control" ["max-age=600"]} at)))
      (is (nil? (headers/not-before {"retry-after" ["soon"]} at))))
    (testing "the values of every Cache-Control header"
      (is (= #inst "2026-09-27T12:10:00Z" (headers/not-before {"cache-control" ["public" "max-age=600"]} at)))
      (is (nil? (headers/not-before {"cache-control" ["max-age=600" "no-store"]} at))))
    (testing "nothing when the headers forbid a cache or name no later time"
      (is (nil? (headers/not-before {"cache-control" ["no-cache, max-age=600"]} at)))
      (is (nil? (headers/not-before {"cache-control" ["s-maxage=600"]} at)) "a shared cache's age is not ours")
      (is (nil? (headers/not-before {"expires" ["Sun, 27 Sep 2026 11:30:00 GMT"]} at)))
      (is (nil? (headers/not-before {} at))))
    (is (= #inst "2026-10-09T01:46:39Z" (headers/not-before {"retry-after" ["999999"]} at))
        "as late as the server says")
    (is (= #inst "2026-09-27T13:00:00Z" (headers/not-before {"retry-after" ["999999"]} at {:max-wait-seconds 3600}))
        "or at most the :max-wait-seconds of the options")
    (is (= #inst "2026-09-27T13:00:00Z"
           (headers/not-before {"cache-control" ["public, max-age=424481"] "age" ["180318"]} at {:max-wait-seconds 3600})))
    (testing "a date with an offset, and one of a day that does not exist"
      (is (= #inst "2026-09-27T12:40:00Z" (headers/not-before {"retry-after" ["Sun, 27 Sep 2026 14:40:00 +0200"]} at)))
      (is (nil? (headers/not-before {"retry-after" ["Sun, 31 Sep 2026 12:40:00 GMT"]} at))))))

(deftest links-of-a-header
  (is (= [{:href "https://hub.example.com/" :rel "hub"} {:href "http://example.com/feed" :rel "self"}]
         (headers/links {"link" "<https://hub.example.com/>; rel=\"hub\", <http://example.com/feed>; rel=\"self\""})))
  (is (= [{:href "https://hub.example.com/?a=1,2" :rel "hub" :title "A, B; C"}
          {:href "/feed.xml" :rel "self alternate" :type "application/rss+xml"}]
         (headers/links {"link" ["<https://hub.example.com/?a=1,2>; rel=hub; title=\"A, B; C\"; REL=other"
                                 "</feed.xml>;rel=\"self alternate\";type=application/rss+xml"]}))
      "several headers, a comma and a semicolon inside a value, and the first value of a parameter")
  (is (= [{:href "x"}] (headers/links {"link" "garbage, <>; rel, <x>; rel"})) "what is no link is left out")
  (is (= [] (headers/links {})))
  (is (= [] (headers/links {"link" (apply str (repeat 20000 "<"))})) "a long header of a stranger"))
