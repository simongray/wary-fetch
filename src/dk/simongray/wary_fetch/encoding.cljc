(ns ^:no-doc dk.simongray.wary-fetch.encoding
  "The encoding of a body in bytes, from its byte order mark, the charset
  of its Content-Type, or what the document says of itself: the meta tag
  of an HTML page, by the HTML Standard, section 13.2.3.2, or the
  declaration of an XML document, by XML 1.0, appendix F, and RFC 7303,
  section 3.2."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.headers :as headers]))

(defn strip-bom
  "The text `s` without a leading byte order mark."
  [s]
  (if (str/starts-with? s "﻿")
    (subs s 1)
    s))

(defn- byte-at
  "The byte at `i` of the bytes `b`, from 0 to 255, or nil past the end."
  [b i]
  (when (< i #?(:clj (alength ^bytes b) :cljs (.-length b)))
    #?(:clj  (bit-and (aget ^bytes b i) 0xff)
       :cljs (aget b i))))

(defn- bom-encoding
  "The encoding that the byte order mark of the bytes `b` names, or nil."
  [b]
  (let [start [(byte-at b 0) (byte-at b 1) (byte-at b 2)]]
    (cond
      (= [0xef 0xbb 0xbf] start)  "UTF-8"
      (= [0xfe 0xff] (pop start)) "UTF-16BE"
      (= [0xff 0xfe] (pop start)) "UTF-16LE")))

(defn- head
  "The first `n` bytes of `b` at most, a character for each byte."
  [b n]
  (let [n (min n #?(:clj (alength ^bytes b) :cljs (.-length b)))]
    #?(:clj  (String. ^bytes b 0 (int n) "ISO-8859-1")
       :cljs (.decode (js/TextDecoder. "latin1") (.subarray b 0 n)))))

(defn- declared-encoding
  "The encoding that the XML declaration of the bytes `b` names, or nil."
  [b]
  ;; the declaration comes first, so the start of the document is enough
  (second (re-find #"(?i)<\?xml[^>]*encoding=[\"']([\w.:-]+)" (head b 200))))

(def attribute-pattern
  "An attribute of a start tag: its name, and its value in double quotes,
  in single quotes, or bare."
  #"([^\s/=>\"']+)(?:\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s>]+)))?")

(defn- attributes
  "The attributes in the text `s` of a start tag after its name, by their
  names in lower case, the first of a name winning."
  [s]
  (reduce (fn [found [_ k double single bare]]
            (let [k (str/lower-case k)]
              (cond-> found
                (not (contains? found k))
                (assoc k (or double single bare "")))))
          {}
          (re-seq attribute-pattern s)))

(defn- content-charset
  "The charset that `s`, the content of a meta tag's pragma, names, or
  nil."
  [s]
  (second (re-find #"(?i)charset\s*=\s*[\"']?([^\s;\"']+)" s)))

;; the HTML Standard, section 13.2.3.2, the prescan of a byte stream, but
;; for a > in the value of an attribute before the meta tag; and the
;; algorithm for extracting a character encoding from a meta element
(defn- meta-encoding
  "The encoding that the first meta tag with one names, in the first 1024
  bytes of the HTML page in the bytes `b`, outside a comment, or nil. A
  UTF-16 label counts as UTF-8, since the page was read as ASCII."
  [b]
  (let [page    (str/replace (head b 1024) #"<!--[\s\S]*?(?:-->|$)" "")
        pragma? #(= "content-type" (str/lower-case (str/trim (str %))))
        label   (fn [tag]
                  (let [attrs (attributes (subs tag 5))]
                    (or (get attrs "charset")
                        (when (pragma? (get attrs "http-equiv"))
                          (some-> (get attrs "content") content-charset)))))
        found   (some label (re-seq #"(?i)<meta[\s/][^>]*" page))]
    (if (some->> found (re-find #"(?i)^\s*utf-16"))
      "UTF-8"
      found)))

;; the Encoding Standard, section 4.2: TextDecoder reads these labels as
;; windows-1252, in Node and in a browser
(def latin-1-labels
  "The labels that name windows-1252, the encoding of Latin-1 and ASCII in
  a browser."
  #{"ansi_x3.4-1968" "ascii" "cp1252" "cp819" "csisolatin1" "ibm819"
    "iso-8859-1" "iso-ir-100" "iso8859-1" "iso88591" "iso_8859-1"
    "iso_8859-1:1987" "l1" "latin1" "us-ascii" "windows-1252" "x-cp1252"})

(defn- encoding-of
  "The encoding that `label` names, as a browser reads it: windows-1252
  for a label of Latin-1 or ASCII, and else `label`."
  [label]
  (if (contains? latin-1-labels (str/lower-case (str/trim label)))
    "windows-1252"
    label))

#?(:cljs
   (def windows-1252-c1
     "The code points of the bytes 0x80 to 0x9F in windows-1252, by the index
     of the Encoding Standard."
     [0x20AC 0x0081 0x201A 0x0192 0x201E 0x2026 0x2020 0x2021
      0x02C6 0x2030 0x0160 0x2039 0x0152 0x008D 0x017D 0x008F
      0x0090 0x2018 0x2019 0x201C 0x201D 0x2022 0x2013 0x2014
      0x02DC 0x2122 0x0161 0x203A 0x0153 0x009D 0x017E 0x0178]))

;; Node's TextDecoder gives the C1 controls for these bytes, against the
;; index; a browser gives the characters already
#?(:cljs
   (defn- with-windows-1252
     "The text `s` that a TextDecoder gave for windows-1252, with each C1
     control in it as the character of windows-1252 of its byte."
     [s]
     (let [c1 (js/RegExp. (str "[" (.fromCharCode js/String 0x80) "-"
                               (.fromCharCode js/String 0x9F) "]")
                          "g")]
       (.replace s c1 (fn [c]
                        (.fromCodePoint js/String
                                        (nth windows-1252-c1
                                             (- (.charCodeAt c 0) 0x80))))))))

(defn- decoded
  "The bytes `b` as text in `encoding`, or in UTF-8 when the platform
  doesn't know it."
  [b encoding]
  #?(:clj  (let [^java.nio.charset.Charset charset
                 (try
                   (java.nio.charset.Charset/forName encoding)
                   (catch Exception _
                     java.nio.charset.StandardCharsets/UTF_8))]
             (String. ^bytes b charset))
     :cljs (cond-> (.decode (try
                              (js/TextDecoder. encoding)
                              (catch :default _
                                (js/TextDecoder. "utf-8")))
                            b)
             (= "windows-1252" encoding) with-windows-1252)))

(defn decode
  "The text of the body in the bytes `b` of a response with `headers`, in
  the encoding that the first of these names, or else UTF-8:

  - the byte order mark
  - the charset of the Content-Type
  - a meta tag in the first 1024 bytes of an HTML page, or the
    declaration of any other document, which XML has

  A label of Latin-1 or ASCII is windows-1252, as a browser reads it, an
  unknown encoding counts as UTF-8, and the byte order mark is dropped."
  [b headers]
  (let [html? (= "text/html"
                 (headers/media-type (headers/header headers "content-type")))]
    (strip-bom (decoded b (encoding-of (or (bom-encoding b)
                                           (headers/charset headers)
                                           (if html?
                                             (meta-encoding b)
                                             (declared-encoding b))
                                           "UTF-8"))))))
