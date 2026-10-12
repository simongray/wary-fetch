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
      (is (= "<a>x</a>" (jvm/decode {"content-type" ["text/xml; charset=nonsense"]} (.getBytes "<a>x</a>" "UTF-8")))))
    (testing "a label of Latin-1 is windows-1252, as in a browser"
      (let [euro (str "<rss>€ " (char 0x201C) "quoted" (char 0x201D) "</rss>")]
        (is (= euro (jvm/decode {"content-type" ["application/rss+xml; charset=ISO-8859-1"]}
                                (.getBytes euro "windows-1252"))))))))

(deftest decoding-html-pages
  (let [html    (fn [meta] (str "<!doctype html><html><head>" meta "<title>Café €</title></head></html>"))
        page    (fn [meta] (.getBytes ^String (html meta) "windows-1252"))
        as-html {"content-type" ["text/html"]}
        decoded #(jvm/decode %1 (page %2))]
    (testing "the meta tag names the charset of a page whose headers don't"
      (is (= (html "<meta charset=\"windows-1252\">") (decoded as-html "<meta charset=\"windows-1252\">")))
      (is (= (html "<META CHARSET=windows-1252>") (decoded as-html "<META CHARSET=windows-1252>")))
      (let [pragma "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"]
        (is (= (html pragma) (decoded as-html pragma)) "or its pragma, where Latin-1 is windows-1252")))
    (testing "but not from a comment, the value of another attribute, past the first 1024 bytes, or for another type"
      (let [quoted "<meta name=\"description\" content=\"a page whose charset=windows-1252\">"]
        (is (not= (html quoted) (decoded as-html quoted))))
      (let [hidden "<!-- <meta charset=\"windows-1252\"> -->"
            late   (str (apply str (repeat 1100 " ")) "<meta charset=\"windows-1252\">")
            meta   "<meta charset=\"windows-1252\">"]
        (is (not= (html hidden) (decoded as-html hidden)))
        (is (not= (html late) (decoded as-html late)))
        (is (not= (html meta) (decoded {"content-type" ["text/plain"]} meta)))))
    (testing "the header wins over the meta tag, and a UTF-16 label is UTF-8"
      (let [meta "<meta charset=\"utf-16\">"]
        (is (= (html meta) (jvm/decode as-html (.getBytes ^String (html meta) "UTF-8"))))
        (is (not= (html "<meta charset=\"windows-1252\">")
                  (decoded {"content-type" ["text/html; charset=UTF-8"]} "<meta charset=\"windows-1252\">")))))))
