(ns ^:no-doc dk.simongray.wary-fetch.encoding
  "The encoding of a body in bytes, from its byte order mark, the charset
  of its Content-Type, or the declaration of an XML document, by XML 1.0,
  appendix F, and RFC 7303, section 3.2."
  (:require [clojure.string :as str]))

(defn strip-bom
  "The text `s` without a leading byte order mark."
  [s]
  (if (str/starts-with? s "\uFEFF")
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
  (let [head [(byte-at b 0) (byte-at b 1) (byte-at b 2)]]
    (cond
      (= [0xef 0xbb 0xbf] head)     "UTF-8"
      (= [0xfe 0xff] (pop head))    "UTF-16BE"
      (= [0xff 0xfe] (pop head))    "UTF-16LE")))

(defn- declared-encoding
  "The encoding that the XML declaration of the bytes `b` names, or nil."
  [b]
  ;; the declaration comes first, so the start of the document is enough
  (let [n    (min 200 #?(:clj (alength ^bytes b) :cljs (.-length b)))
        head #?(:clj  (String. ^bytes b 0 (int n) "ISO-8859-1")
                :cljs (.decode (js/TextDecoder. "latin1") (.subarray b 0 n)))]
    (second (re-find #"(?i)<\?xml[^>]*encoding=[\"']([\w.:-]+)" head))))

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
     :cljs (.decode (try
                      (js/TextDecoder. encoding)
                      (catch :default _
                        (js/TextDecoder. "utf-8")))
                    b)))

(defn decode
  "The text of the XML document in the bytes `b`, in the encoding that its
  byte order mark, `charset` or XML declaration names first, or else UTF-8.
  An unknown encoding counts as UTF-8, and the byte order mark is dropped."
  [b charset]
  (strip-bom (decoded b (or (bom-encoding b)
                            charset
                            (declared-encoding b)
                            "UTF-8"))))
