(ns ^:no-doc dk.simongray.wary-fetch.encoding
  "The encoding of a body in bytes, from its byte order mark, the charset
  of its Content-Type or what the document declares: an HTML page's meta
  tag (HTML Standard, section 13.2.3.2) or an XML document's declaration
  (XML 1.0, appendix F, and RFC 7303, section 3.2)."
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-mime :as mime]))

(defn- head
  "The first `n` bytes of `b` at most, a character for each byte."
  [b n]
  (let [n (min n #?(:clj (alength ^bytes b) :cljs (.-length b)))]
    ;; by the byte, since the TextDecoder of Hermes knows only UTF-8
    #?(:clj  (String. ^bytes b 0 (int n) "ISO-8859-1")
       :cljs (.apply (.-fromCharCode js/String) nil (.subarray b 0 n)))))

(defn- declared-encoding
  "The encoding that the XML declaration of the bytes `b` names, or nil."
  [b]
  ;; the declaration comes first, so the start of the document is enough
  (some-> (re-find #"(?i)<\?xml[^>]*encoding=[\"']([\w.:-]+)" (head b 200))
          second
          mime/label->encoding))

(def attribute-pattern
  "An attribute of a start tag: its name, and its value in double quotes,
  in single quotes, or bare."
  #"([^\s/=>\"']+)(?:\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s>]+)))?")

(defn- attributes
  "The attributes in the text `s` of a start tag after its name, by their
  names in lower case. When a name repeats, the first one wins."
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
  "The encoding that the first meta tag with one names in the first 1024
  bytes of the HTML page in the bytes `b`, outside comments, or nil.
  UTF-16 counts as UTF-8, since the page was read as ASCII, and
  x-user-defined counts as windows-1252."
  [b]
  (let [page    (str/replace (head b 1024) #"<!--[\s\S]*?(?:-->|$)" "")
        pragma? #(= "content-type" (str/lower-case (str/trim (str %))))
        label   (fn [tag]
                  (let [attrs (attributes (subs tag 5))]
                    (or (get attrs "charset")
                        (when (pragma? (get attrs "http-equiv"))
                          (some-> (get attrs "content") content-charset)))))
        found   (some #(some-> (label %) mime/label->encoding)
                      (re-seq #"(?i)<meta[\s/][^>]*" page))]
    (case found
      ("UTF-16BE" "UTF-16LE") "UTF-8"
      "x-user-defined"        "windows-1252"
      found)))

(defn decode
  "The text of the body in the bytes `b` of a response with `headers`, in
  the encoding that the first of these names, or else UTF-8:

  - the byte order mark
  - the charset of the Content-Type
  - a meta tag in the first 1024 bytes of an HTML page, or the XML
    declaration of any other document

  Each label names an encoding as in a browser, e.g. latin1 names
  windows-1252. A label of no encoding doesn't count. The text leaves out
  the byte order mark."
  [b headers]
  (let [content-type (mime/extract (get headers "content-type"))]
    ;; the byte order mark comes first, as mime/decode reads it
    (mime/decode b (or (mime/encoding content-type)
                       (if (mime/html? content-type)
                         (meta-encoding b)
                         (declared-encoding b))))))
