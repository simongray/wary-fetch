(ns ^:no-doc dk.simongray.wary-fetch.limits
  "The limits of an exchange, which every client keeps: where a request may
  go, what its headers may hold, how long it may take and how much of a
  body it reads, and the exceptions of the requests that go beyond them or
  never reach a server."
  (:require [dk.simongray.wary-fetch :as-alias http]
            [dk.simongray.wary-fetch.secret :as secret]))

(def default-max-bytes
  "How much of a response body is read at most, unless the request sets
  another limit: 100 MB."
  (* 100 1024 1024))

(defn refusal
  "The exception of a request to `url` that :allow-pred refused."
  [url]
  (ex-info (str "The request to " url " is not allowed")
           {:type ::http/refused :url url}))

;; RFC 9110, section 5.5. The JDK and node:http refuse these characters too
;; (fetch refuses fewer), so none of them throws with the value.
(defn unsendable-header
  "The name of the first request header in `headers` whose value has a
  character that HTTP doesn't allow (e.g. a line break), or nil for none."
  [headers]
  (some (fn [[k v]]
          (when (re-find #"[^\t\x20-\x7E\x80-\xFF]" (str (secret/reveal v)))
            k))
        headers))

(defn invalid-header
  "The exception of a request to `url` whose `headers` have one that
  unsendable-header finds. It names that header but leaves out the value,
  which can be a secret (e.g. a token read from a file with its line
  break)."
  [url headers]
  (let [k (unsendable-header headers)]
    (ex-info (str "The request to " url " has a character in its " k
                  " header that HTTP doesn't allow")
             {:type ::http/invalid-header :url url :header k})))

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

(defn aborted
  "The exception of a request to `url` that its :signal stopped."
  [url]
  (ex-info (str "The request to " url " was aborted")
           {:type ::http/aborted :url url}))

(defn stopped?
  "Whether the :signal `signal` of a request says to stop it: in
  JavaScript an AbortSignal that aborted, and on the JVM a value whose
  deref is truthy, without waiting for one that isn't realized yet, such
  as a promise."
  [signal]
  (boolean
   (when signal
     #?(:clj  (if (instance? clojure.lang.IPending signal)
                (and (realized? signal) @signal)
                @signal)
        :cljs (.-aborted ^js signal)))))

(defn unresolved
  "The exception of a request to `url` whose host name doesn't resolve,
  with the `cause` that the platform gave, or nil."
  [url cause]
  (ex-info (str "The host of " url " was not found")
           {:type ::http/unresolved :url url}
           cause))

(defn unreachable
  "The exception of a request to `url` whose host couldn't be reached, with
  the `cause` that the platform gave."
  [url cause]
  (ex-info (str "The host of " url " could not be reached")
           {:type ::http/unreachable :url url}
           cause))

(defn tls-failure
  "The exception of a request to `url` whose secure connection failed, with
  the `cause` that the platform gave and its `reason`, e.g. an expired
  certificate."
  [url cause reason]
  (ex-info (str "The secure connection to " url " failed"
                (when reason (str ": " reason)))
           {:type ::http/tls :url url}
           cause))

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
