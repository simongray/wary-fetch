(ns dk.simongray.wary-fetch.bytes-test
  "Bytes, their encodings, hashes and HMACs, the same on both platforms,
  by the test vectors of their standards."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.wary-fetch.bytes :as bytes]))

(deftest digests-and-hmacs
  (testing "FIPS 180-4, the digests of abc"
    (is (= "a9993e364706816aba3e25717850c26c9cd0d89d" (bytes/hex (bytes/digest :sha1 (bytes/encode "abc")))))
    (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
           (bytes/hex (bytes/digest :sha256 (bytes/encode "abc")))))
    (is (= 48 (bytes/length (bytes/digest :sha384 (bytes/encode "abc")))))
    (is (= 64 (bytes/length (bytes/digest :sha512 (bytes/encode "abc"))))))
  (testing "RFC 4231, test case 2"
    (is (= "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
           (bytes/hex (bytes/hmac :sha256 (bytes/encode "Jefe") (bytes/encode "what do ya want for nothing?")))))))

(deftest encodings
  (let [b (bytes/of [0 1 2 250 251 252 253 254 255])]
    (is (= "AAEC-vv8_f7_" (bytes/base64url b)) "the URL-safe alphabet, without padding")
    (is (= "AAEC+vv8/f7/" (bytes/base64 b)))
    (is (= (seq (map #(bit-and % 0xff) b)) (seq (map #(bit-and % 0xff) (bytes/unbase64url (bytes/base64url b))))))
    (is (nil? (bytes/unbase64url "not base64!")))
    (is (= "000102fafbfcfdfeff" (bytes/hex b))))
  (is (= "Blåbær" (bytes/decode (bytes/encode "Blåbær"))))
  (is (= 16 (bytes/length (bytes/random 16))))
  (testing "comparisons of secrets"
    (is (bytes/same-text? "s3cr3t" "s3cr3t"))
    (is (not (bytes/same-text? "s3cr3t" "s3cr3T")))
    (is (not (bytes/same-text? "s3cr3t" nil)))
    (is (not (bytes/same? (bytes/encode "a") (bytes/encode "ab"))))))
