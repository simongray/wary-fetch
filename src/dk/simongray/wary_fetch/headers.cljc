(ns dk.simongray.wary-fetch.headers
  "The values of HTTP headers as data: the header maps of responses, links,
  byte ranges, and when to ask again.

  A map of headers has lower-case names, and each value is a string or a
  vector of strings."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.date :as date]))

(defn header
  "The first value of `header-name` in the response headers map `headers`,
  whose values are strings or vectors of them."
  [headers header-name]
  (let [v (get headers (str/lower-case header-name))]
    (if (string? v) v (first v))))

(defn joined
  "All the values of `header-name` in the response headers map `headers`,
  joined by commas as fetch joins them, or nil for none."
  [headers header-name]
  (let [v (get headers (str/lower-case header-name))]
    (if (string? v)
      v
      (some->> (seq v) (str/join ", ")))))

(defn- link-of
  "The link that the `tokens` of one value of a Link header give, the
  target and then the parameters, each after a semicolon. Nil when the
  value starts with no target."
  [[target & params :as tokens]]
  (when (and target
             (re-find #"^<\s*[^\s>]" target))
    (reduce (fn [link [k eq v]]
              (let [k (keyword (str/lower-case k))]
                (cond-> link
                  (and (= "=" eq)
                       v
                       (not (contains? link k)))
                  (assoc k (str/replace v #"^\"|\"$" "")))))
            {:href (str/trim (subs target 1 (dec (count target))))}
            (remove #(= ";" (first %)) (partition-by #(= ";" %) params)))))

(defn links
  "The links of the Link header in the response `headers`, as section 3
  of RFC 8288 defines them.

  Each link is a map with its target as :href, and with its parameters
  by name, e.g. :rel and :type. The links are in the order of the header.
  The first value of a parameter counts, and a target can be relative to
  the URL of the response."
  [headers]
  (->> (re-seq #"<[^>]*>|\"[^\"]*\"|[;,=]|[^\s;,=<\"]+"
               (str (joined headers "link")))
       (partition-by #(= "," %))
       (keep link-of)
       vec))

;; RFC 9110, section 8.4.1.3: x-gzip is gzip
(defn gzipped?
  "Whether the response with `headers` says that its body is gzipped."
  [headers]
  (let [coding (some-> (header headers "content-encoding")
                       str/trim
                       str/lower-case)]
    (contains? #{"gzip" "x-gzip"} coding)))

;; RFC 9110, section 14.4
(defn content-range
  "The byte range and the size of the whole file that the Content-Range
  header value `s` names, e.g. bytes 0-1023/146515, as a map of :start,
  :end and :total.

  It's nil when `s` doesn't read, and the :total is nil when the server
  doesn't know the size."
  [s]
  (let [pattern #"(?i)\s*bytes\s+(\d{1,15})-(\d{1,15})/(\d{1,15}|\*)\s*"]
    (when-let [[_ start end total] (some->> s (re-matches pattern))]
      {:start (parse-long start)
       :end   (parse-long end)
       :total (when (not= "*" total)
                (parse-long total))})))

(defn- byte-range
  "The range that `spec`, one byte range of a Range header, asks for, as a
  pair of its start and end, or nil when it doesn't read."
  [spec]
  (when-let [[_ start end] (re-matches #"(\d{0,15})\s*-\s*(\d{0,15})" spec)]
    (cond
      (and (seq start) (seq end))
      (let [a (parse-long start)
            b (parse-long end)]
        (when (<= a b) [a b]))

      (seq start)
      [(parse-long start) nil]

      (seq end)
      [nil (parse-long end)])))

(defn byte-ranges
  "The byte ranges that the Range header value `s` asks for, as RFC 9110,
  section 14.1.2, reads them. Nil when `s` doesn't read, because a server
  then ignores it.

  A value such as bytes=0-1023, 5000- asks for two ranges. A range is a
  pair of its start and its end, with the end included, or nil for the
  rest of the file. A nil start and an end of n ask for the last n
  bytes."
  [s]
  (when-let [[_ specs] (some->> s (re-matches #"(?i)\s*bytes\s*=\s*(.*)"))]
    (let [ranges (for [spec (map str/trim (str/split specs #","))
                       :when (seq spec)]
                   (byte-range spec))]
      (when (and (seq ranges)
                 (every? some? ranges))
        (vec ranges)))))

(defn ^:no-doc seconds
  "The whole number of seconds that the header value `s` gives, or nil
  when it gives none."
  [s]
  (some->> s str/trim (re-matches #"\d{1,10}") parse-long))

(defn ^:no-doc ours
  "The time `t` on the clock of the server, as a time on the clock of the
  client. The response that `arrived` names `served` as the time that the
  server sent it. Without `served`, the clocks count as one."
  [t arrived served]
  (if served
    (+ arrived (- t served))
    t))

;; RFC 9110, section 10.2.3
(defn- retry-after
  "When the Retry-After of `headers` says to ask again, in epoch
  milliseconds, by the times that the response `arrived` and was `served`.
  Nil when it gives no time."
  [headers arrived served]
  (let [retry (header headers "retry-after")]
    (or (some->> (seconds retry) (* 1000) (+ arrived))
        (some-> (date/epoch-ms retry) (ours arrived served)))))

;; RFC 9111, section 4.2.1
(defn- fresh-until
  "When the freshness of the response with `headers` ends, in epoch
  milliseconds, by the times that the response `arrived` and was `served`.
  Nil when the headers forbid a cache or name no time.

  The freshness ends at the max-age less the Age, or else at the Expires."
  [headers arrived served]
  (let [age     (* 1000 (or (seconds (header headers "age")) 0))
        cache   (some-> (joined headers "cache-control") str/lower-case)
        max-age (some->> cache
                         (re-find #"max-age=(\d{1,10})")
                         second
                         parse-long)
        expires (date/epoch-ms (header headers "expires"))]
    (when-not (some->> cache (re-find #"no-cache|no-store"))
      (if max-age
        (- (+ arrived (* 1000 max-age)) age)
        (some-> expires
                (ours arrived served)
                (- (if served age 0)))))))

(defn- instant
  "The instant of the epoch milliseconds `ms`, as the platform keeps it."
  [ms]
  #?(:clj  (java.util.Date. (long ms))
     :cljs (js/Date. ms)))

(defn not-before
  "The earliest instant to ask again, by the response `headers` that
  arrived at `at`, with the wait capped at the :max-wait-seconds of `opts`.

  The instant comes from the first of these that the headers give: the
  Retry-After, the max-age of Cache-Control less the Age, or the Expires.
  It's nil when they name no later time or forbid a cache. A date counts
  from the Date of the response so that a wrong clock on the server
  doesn't matter (RFC 9111, section 4.2.1)."
  ([headers at]
   (not-before headers at {}))
  ([headers at {:keys [max-wait-seconds] :as opts}]
   (let [arrived (inst-ms at)
         served  (date/epoch-ms (header headers "date"))
         until   (or (retry-after headers arrived served)
                     (fresh-until headers arrived served))]
     (when (and until (> until arrived))
       (instant (cond-> until
                  max-wait-seconds (min (+ arrived
                                           (* 1000 max-wait-seconds)))))))))

(comment
  (byte-ranges "bytes=0-1023, 5000-")

  ;; an hour at most, though the server says a day
  (not-before {"cache-control" "max-age=86400"} #inst "2026-09-29T12:00:00Z"
              {:max-wait-seconds 3600})
  #_.)
