(ns ^:no-doc dk.simongray.wary-fetch.limits
  "The limits of an exchange, which every client keeps: where a request may
  go, how long it may take and how much of a body it reads, and the
  exceptions of the requests that go beyond them."
  (:require [dk.simongray.wary-fetch :as-alias http]))

(def default-max-bytes
  "How much of a response body is read at most, unless the request sets
  another limit: 100 MB."
  (* 100 1024 1024))

(defn refusal
  "The exception of a request to `url` that :allow-pred refused."
  [url]
  (ex-info (str "The request to " url " is not allowed")
           {:type ::http/refused :url url}))

(defn too-large
  "The exception of a response body from `url` that holds more than
  `max-bytes`."
  [url max-bytes]
  (ex-info (str "Response body exceeds " max-bytes " bytes: " url)
           {:type ::http/too-large :url url :max-bytes max-bytes}))

(defn late
  "The exception of a request to `url` that didn't end in time."
  [url]
  (ex-info (str "The request did not end in time: " url)
           {:type ::http/timeout :url url}))

(defn prefix-length
  "How much of the body of an answer of `status` to read for a request of
  the bytes `range`, or nil to read all of it. The answer sends from
  `start`, it can be `gzipped?`, and the body is read to `max-bytes`.

  - Of a 206, it's the bytes from the start to the end of the range,
    since a server can send more than was asked.
  - Of a 200 that isn't gzipped, which is the whole file as it is, it's
    the bytes from its start to the end of the range.
  - Of any other answer that isn't gzipped, e.g. the page of a 404, it's
    as many bytes as the range names.

  It's nil when the range has no end, or when the prefix is longer than
  `max-bytes`."
  [status start gzipped? range max-bytes]
  (let [end  (second range)
        from (cond
               (= 206 status) (or start (first range))
               gzipped?       nil
               (= 200 status) 0
               :else          (first range))]
    (when (and end from)
      (let [n (max 0 (inc (- end from)))]
        (when (<= n (or max-bytes 0x7FFFFFFF))
          n)))))
