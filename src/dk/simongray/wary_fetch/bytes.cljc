(ns dk.simongray.wary-fetch.bytes
  "Bytes on both platforms, a byte array on the JVM and a Uint8Array in
  JavaScript, with the encodings, hashes and randomness that requests and
  their signatures need.

  The hashes are synchronous on both platforms. In JavaScript they come
  from the Closure Library rather than WebCrypto, whose digests only come
  as promises."
  (:refer-clojure :exclude [concat])
  #?(:cljs (:require [goog.crypt :as crypt]
                     [goog.crypt.base64 :as base64]
                     [goog.crypt.Hmac]
                     [goog.crypt.Sha1]
                     [goog.crypt.Sha256]
                     [goog.crypt.Sha384]
                     [goog.crypt.Sha512]))
  #?(:clj (:import [java.io ByteArrayOutputStream]
                   [java.nio.charset StandardCharsets]
                   [java.security MessageDigest SecureRandom]
                   [java.util Base64]
                   [javax.crypto Mac]
                   [javax.crypto.spec SecretKeySpec])))

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

(defn concat
  "The bytes of each of `bs`, a collection of bytes, one after the other."
  [bs]
  #?(:clj  (let [out (ByteArrayOutputStream.)]
             (doseq [^bytes b bs]
               (.write out b 0 (alength b)))
             (.toByteArray out))
     :cljs (let [out (js/Uint8Array. (transduce (map length) + bs))]
             (reduce (fn [i b] (.set out b i) (+ i (length b))) 0 bs)
             out)))

(defn encode
  "The text `s` as the bytes of its UTF-8."
  [s]
  #?(:clj  (.getBytes (str s) StandardCharsets/UTF_8)
     :cljs (.encode (js/TextEncoder.) (str s))))

(defn decode
  "The text of the bytes `b` in UTF-8."
  [b]
  #?(:clj  (String. ^bytes b StandardCharsets/UTF_8)
     :cljs (.decode (js/TextDecoder.) b)))

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

;; RFC 4648, section 5
(defn base64url
  "The bytes `b` in the URL-safe alphabet of base64, without padding."
  [b]
  #?(:clj  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) ^bytes b)
     :cljs (base64/encodeByteArray b base64/Alphabet.WEBSAFE_NO_PADDING)))

(defn unbase64url
  "The bytes of the text `s` in the URL-safe alphabet of base64, with or
  without padding, or nil when it isn't."
  [s]
  (when (and (string? s) (re-matches #"[A-Za-z0-9_-]*={0,2}" s))
    (try
      #?(:clj  (.decode (Base64/getUrlDecoder) ^String s)
         :cljs (base64/decodeStringToUint8Array s))
      (catch #?(:clj Exception :cljs :default) _
        nil))))

(defn hex
  "The bytes `b` in hexadecimal, two characters in lower case for each."
  [b]
  #?(:clj  (let [sb (StringBuilder.)]
             (doseq [x ^bytes b]
               (.append sb (format "%02x" (bit-and (long x) 0xff))))
             (str sb))
     :cljs (crypt/byteArrayToHex b)))

(defn same?
  "Whether the bytes `a` and `b` are the same, in a time that doesn't tell
  where they differ, as a comparison of secrets needs."
  [a b]
  #?(:clj  (MessageDigest/isEqual ^bytes a ^bytes b)
     :cljs (and (= (alength a) (alength b))
                (zero? (areduce a i acc 0
                                (bit-or acc
                                        (bit-xor (aget a i) (aget b i))))))))

(defn same-text?
  "Whether `a` and `b` are the same text, in a time that doesn't tell where
  they differ. Nil or another value is never the same."
  [a b]
  (and (string? a)
       (string? b)
       (same? (encode a) (encode b))))

(defn random
  "`n` random bytes from the platform's secure source."
  [n]
  #?(:clj  (let [b (byte-array n)]
             (.nextBytes (SecureRandom.) b)
             b)
     :cljs (js/crypto.getRandomValues (js/Uint8Array. n))))

#?(:cljs
   (defn- hasher
     "A hasher of the Closure Library for `algorithm`, and the block size of
     its HMAC, as a pair."
     [algorithm]
     (case algorithm
       :sha1   [(goog.crypt.Sha1.) 64]
       :sha256 [(goog.crypt.Sha256.) 64]
       :sha384 [(goog.crypt.Sha384.) 128]
       :sha512 [(goog.crypt.Sha512.) 128])))

#?(:clj
   (defn- java-name
     "The name that the JDK gives the hash `algorithm`, e.g. SHA-256 for
     :sha256."
     [algorithm]
     (case algorithm
       :sha1   "SHA-1"
       :sha256 "SHA-256"
       :sha384 "SHA-384"
       :sha512 "SHA-512")))

;; FIPS 180-4
(defn digest
  "The digest of the bytes `b` by `algorithm`, :sha1, :sha256, :sha384 or
  :sha512, as bytes."
  [algorithm b]
  #?(:clj  (.digest (MessageDigest/getInstance (java-name algorithm)) ^bytes b)
     :cljs (let [[h] (hasher algorithm)]
             (.update h b)
             (js/Uint8Array.from (.digest h)))))

;; RFC 2104
(defn hmac
  "The HMAC of the bytes `message` with the bytes `key` by `algorithm`,
  :sha1, :sha256, :sha384 or :sha512, as bytes."
  [algorithm key message]
  #?(:clj  (let [mac (str "Hmac" (.toUpperCase (name algorithm)))]
             (.doFinal (doto (Mac/getInstance mac)
                         (.init (SecretKeySpec. ^bytes key mac)))
                       ^bytes message))
     :cljs (let [[h block] (hasher algorithm)
                 mac       (goog.crypt.Hmac. h (js/Array.from key) block)]
             (js/Uint8Array.from (.getHmac mac (js/Array.from message))))))

(comment
  (hex (digest :sha256 (encode "abc")))
  ;; => "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  (base64url (hmac :sha256 (encode "key") (encode "message")))
  (same-text? "secret" "secret")
  #_.)
