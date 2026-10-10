(ns ^:no-doc dk.simongray.wary-fetch.bytes
  "Bytes on both platforms: a byte array on the JVM, and a Uint8Array in
  JavaScript."
  #?(:clj (:import [java.nio.charset StandardCharsets]
                   [java.util Base64])))

(defn length
  "The number of bytes in `b`."
  [b]
  #?(:clj  (alength ^bytes b)
     :cljs (.-length b)))

(defn of
  "The bytes `xs`, numbers from 0 to 255, as the platform keeps bytes."
  [xs]
  #?(:clj  (byte-array (map unchecked-byte xs))
     :cljs (js/Uint8Array.from (into-array xs))))

(defn encode
  "The text `s` as the bytes of its UTF-8."
  [s]
  #?(:clj  (.getBytes ^String s StandardCharsets/UTF_8)
     :cljs (.encode (js/TextEncoder.) s)))

#?(:cljs
   (defn- binary-string
     "The bytes `b` as a string of one character for each byte, in pieces so
     that a large body never overflows the stack of `apply`."
     [b]
     (let [n (alength b)]
       (apply str (for [i (range 0 n 32768)]
                    (.apply js/String.fromCharCode nil
                            (.subarray b i (min n (+ i 32768)))))))))

;; RFC 4648, section 4
(defn base64
  "The bytes `b` in base64."
  [b]
  #?(:clj  (.encodeToString (Base64/getEncoder) ^bytes b)
     :cljs (js/btoa (binary-string b))))
