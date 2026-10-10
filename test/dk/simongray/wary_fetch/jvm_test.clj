(ns dk.simongray.wary-fetch.jvm-test
  "The bodies that the client of the JVM decodes: their charset, by the
  headers, the byte order mark and the XML declaration, and gzip."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.wary-fetch.jvm :as jvm])
  (:import [java.io ByteArrayOutputStream]
           [java.util.zip GZIPOutputStream]))

(defn- gzip
  ^bytes [^bytes body]
  (let [out (ByteArrayOutputStream.)]
    (with-open [z (GZIPOutputStream. out)]
      (.write z body))
    (.toByteArray out)))

(deftest decoding-bodies
  (let [xml   "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><rss><channel><title>Blåbær</title></channel></rss>"
        latin (.getBytes xml "ISO-8859-1")
        bom   (str (char 0xFEFF))]
    (testing "the XML declaration names the charset when the headers do not"
      (is (= xml (jvm/decode {} latin))))
    (testing "the Content-Type header wins over the declaration"
      (is (not= xml (jvm/decode {"content-type" ["text/xml; charset=UTF-8"]} latin))))
    (testing "a byte order mark wins over the header, as RFC 7303 has it"
      (let [utf-16 (.getBytes (str bom xml) "UTF-16LE")
            utf-8  (.getBytes (str bom xml) "UTF-8")]
        (is (= xml (jvm/decode {"content-type" ["text/xml; charset=ISO-8859-1"]} utf-16)))
        (is (= xml (jvm/decode {"content-type" ["text/xml; charset=ISO-8859-1"]} utf-8)))
        (is (= xml (jvm/decode {} (.getBytes (str bom xml) "UTF-16BE"))))))
    (testing "gzip is undone before decoding, however the header spells it"
      (is (= xml (jvm/decode {"content-encoding" ["gzip"]} (gzip latin))))
      (is (= xml (jvm/decode {"content-encoding" ["GZIP"]} (gzip latin))))
      (is (= xml (jvm/decode {"content-encoding" ["x-gzip"]} (gzip latin)))))
    (testing "an unknown charset falls back to UTF-8"
      (is (= "<a>x</a>" (jvm/decode {"content-type" ["text/xml; charset=nonsense"]} (.getBytes "<a>x</a>" "UTF-8")))))))
