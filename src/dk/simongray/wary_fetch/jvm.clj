(ns ^:no-doc dk.simongray.wary-fetch.jvm
  "The client of the JVM: java.net.http, with redirects followed here
  rather than by the JDK, and bodies read within the limits of time and
  size."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.encoding :as encoding]
            [dk.simongray.wary-fetch.headers :as headers]
            [dk.simongray.wary-fetch.limits :as limits]
            [dk.simongray.wary-fetch.redirect :as redirect]
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.wary-fetch.url :as url])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream InputStream
                    IOException]
           [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest
                          HttpRequest$Builder HttpRequest$BodyPublishers
                          HttpResponse HttpResponse$BodyHandlers
                          HttpTimeoutException]
           [java.security MessageDigest]
           [java.time Duration]
           [java.util.concurrent ScheduledFuture ScheduledThreadPoolExecutor
                                 ThreadFactory TimeUnit]
           [java.util.zip GZIPInputStream]))

(defonce ^{:doc "The shared HTTP client, built on first use, with a
  connect timeout of 15 seconds. Redirects are left to send!, which
  follows them."}
  client
  (delay (-> (HttpClient/newBuilder)
             (.followRedirects HttpClient$Redirect/NEVER)
             (.connectTimeout (Duration/ofSeconds 15))
             (.build))))

(defn headers-of
  "The headers of `response`, as a map from lower-case names to vectors of
  values."
  [^HttpResponse response]
  (into {} (map (fn [[k v]] [(str/lower-case k) (vec v)]))
        (.map (.headers response))))

(defn pump
  "Read `in` from `url` to its end in pieces, give each piece to `f`, and
  throw ::too-large once more than `max-bytes` have arrived.

  The `f` takes the buffer and the length of the piece. With nil
  `max-bytes`, it reads everything."
  [^InputStream in max-bytes url f]
  (let [buffer (byte-array 65536)]
    (loop [total 0]
      (let [n (.read in buffer)]
        (when-not (neg? n)
          (let [total (+ total n)]
            (when (and max-bytes (> total max-bytes))
              (throw (limits/too-large url max-bytes)))
            (f buffer n)
            (recur total)))))))

(defn read-bounded
  "The bytes of `in` from `url`, read to its end within `max-bytes`."
  ^bytes [^InputStream in max-bytes url]
  (let [out (ByteArrayOutputStream.)]
    (pump in max-bytes url (fn [^bytes buffer n]
                             (.write out buffer 0 (int n))))
    (.toByteArray out)))

(defonce ^{:doc "The daemon thread that closes the body of a response
  still being read when its time is up, built on first use."}
  watchdog
  (delay (doto (ScheduledThreadPoolExecutor.
                1 (reify ThreadFactory
                    (newThread [_ runnable]
                      (doto (Thread. runnable "wary-fetch watchdog")
                        (.setDaemon true)))))
           (.setRemoveOnCancelPolicy true))))

(defn within
  "The result of `f`, which reads `in` from `url`, but only until the
  instant `deadline` in epoch milliseconds.

  At the deadline, the stream is closed and ::timeout thrown, so that a
  server that sends its body slowly holds no thread for longer."
  [^InputStream in deadline url f]
  (let [late?   (atom false)
        wait-ms (long (max 0 (- deadline (System/currentTimeMillis))))
        task    (.schedule ^ScheduledThreadPoolExecutor @watchdog
                           ^Runnable (fn [] (reset! late? true) (.close in))
                           wait-ms
                           TimeUnit/MILLISECONDS)]
    (try
      (let [result (f)]
        ;; a stream closed under a read can end as if the body had ended
        (if @late? (throw (limits/late url)) result))
      (catch IOException e
        (throw (if @late? (limits/late url) e)))
      (finally
        (.cancel ^ScheduledFuture task false)))))

(defn- read-within
  "The bytes of `in` from `url` within `max-bytes`, but only until the
  `deadline`."
  ^bytes [^InputStream in deadline max-bytes url]
  (within in deadline url #(read-bounded in max-bytes url)))

(defn- gunzip
  "The gzipped `body` from `url` inflated, with at most `max-bytes` of the
  result read."
  ^bytes [^bytes body max-bytes url]
  (with-open [in (GZIPInputStream. (ByteArrayInputStream. body))]
    (read-bounded in max-bytes url)))

(defn inflated
  "The `body` bytes of the response with `headers`, gunzipped when the
  response says so, to at most `max-bytes`, or without a limit for nil."
  ^bytes [headers ^bytes body max-bytes]
  (if (headers/gzipped? headers)
    (gunzip body max-bytes nil)
    body))

(defn decode
  "The `body` bytes of the response with `headers` as a string, gunzipped
  to at most `max-bytes` when the response says so.

  With nil `max-bytes`, there's no limit. The body is decoded as an XML
  document is, with the charset that the Content-Type names."
  ([headers body]
   (decode headers body nil))
  ([headers ^bytes body max-bytes]
   (encoding/decode (inflated headers body max-bytes)
                    (headers/charset headers))))

(defn- java-request
  "The java.net.http request of `method` to `uri`, with `headers`, the
  `body` string and a timeout of `timeout-ms` milliseconds.

  Gzip is accepted unless the headers name an Accept-Encoding. A header
  and a body that are hidden texts go out as their texts."
  ^HttpRequest [^URI uri method headers body timeout-ms]
  (let [gzip?   (not-any? #(= "accept-encoding" (str/lower-case (str %)))
                          (keys headers))
        add     (fn [^HttpRequest$Builder b k v]
                  (.header b (str k) (str (secret/reveal v))))
        builder (cond-> (.timeout (HttpRequest/newBuilder uri)
                                  (Duration/ofMillis timeout-ms))
                  gzip? (.header "Accept-Encoding" "gzip"))
        builder (reduce-kv add builder headers)]
    (-> (if (or (nil? method) (= :get method))
          (.GET ^HttpRequest$Builder builder)
          (.method ^HttpRequest$Builder builder
                   (str/upper-case (name method))
                   (let [body (secret/reveal body)]
                     (cond
                       (nil? body)   (HttpRequest$BodyPublishers/noBody)
                       (bytes? body) (HttpRequest$BodyPublishers/ofByteArray
                                      body)
                       :else         (HttpRequest$BodyPublishers/ofString
                                      (str body))))))
        (.build))))

(defn send-following!
  "Send the `request`, follow its redirects until the instant `deadline`
  in epoch milliseconds, and check each URL with `allow-pred`.

  It gives the last response and the redirects before it, each as a
  vector of its status, URL and headers. It follows the redirects itself,
  since the JDK sends credentials on to another origin. A URL that
  `allow-pred` refuses throws ::refused."
  [request deadline allow-pred]
  (loop [request (update request :url url/legal-url)
         chain   []]
    (let [{:keys [url method headers body]} request
          uri  (URI. url)
          left (- deadline (System/currentTimeMillis))]
      (when-not (pos? left)
        (throw (HttpTimeoutException. (str "request timed out: " uri))))
      (when-not (allow-pred url)
        (throw (limits/refusal url)))
      (let [outgoing (java-request uri method headers body left)
            response (.send ^HttpClient @client outgoing
                            (HttpResponse$BodyHandlers/ofInputStream))
            status   (.statusCode response)
            location (-> (.headers response)
                         (.firstValue "location")
                         (.orElse nil))
            limit    (:max-redirects request redirect/limit)
            target   (when (and (redirect/statuses status)
                                (< (count chain) limit))
                       (some-> (redirect/target url location) url/legal-url))]
        (if-not target
          [response chain]
          (do (.close ^InputStream (.body response))
              (recur (redirect/next-request request status target)
                     (conj chain [status url (headers-of response)]))))))))

(defn- response-bytes
  "The bytes of the body of `response` to `request`, read until `deadline`,
  no more than the range of the request needs."
  ^bytes [^HttpResponse response
          {:keys [method url range max-bytes] :as request}
          deadline]
  (let [headers (headers-of response)
        start   (:start (headers/content-range
                         (headers/header headers "content-range")))
        n       (limits/prefix-length (.statusCode response) start
                                      (headers/gzipped? headers) range
                                      max-bytes)]
    (with-open [^InputStream in (.body response)]
      (cond
        ;; an answer to HEAD has no content by RFC 9110, section 9.3.2,
        ;; though some servers send one over HTTP/2
        (= :head method) (byte-array 0)
        n                (within in deadline url #(.readNBytes in (int n)))
        :else            (read-within in deadline max-bytes url)))))

(defn- answer
  "The response map for the `response` of the JDK to `request`, with the
  body `bytes`, which came after the redirects `chain`."
  [^HttpResponse response ^bytes bytes chain
   {:keys [as max-bytes] :as request}]
  (let [status   (.statusCode response)
        headers  (headers-of response)
        final    (str (.uri response))
        moved    (redirect/permanent-url chain final)
        received (headers/content-range
                  (headers/header headers "content-range"))]
    (cond-> {:status  status
             :url     final
             :headers headers
             :body    (when (and (not= 304 status) (pos? (alength bytes)))
                        (if (= :bytes as)
                          (inflated headers bytes max-bytes)
                          (decode headers bytes max-bytes)))}
      moved       (assoc :permanent-url moved)
      (seq chain) (assoc :redirects chain)
      received    (assoc :content-range received))))

(defn send!
  "Send the `request` within `timeout-seconds`, to URLs that `allow-pred`
  takes, and give the response."
  [request timeout-seconds allow-pred]
  (let [deadline         (+ (System/currentTimeMillis)
                            (* 1000 timeout-seconds))
        [response chain] (send-following! request deadline allow-pred)
        bytes            (response-bytes response request deadline)]
    (answer response bytes chain request)))

(defn digests!
  "Send the `request` within `timeout-seconds`, to URLs that `allow-pred`
  takes, and give the :status, the :url and, for a 200, the :digests of
  the body by the MessageDigest `algorithms`, as a map of each algorithm
  to the bytes of its digest. The body streams through the digests, and
  none of it is kept."
  [request algorithms timeout-seconds allow-pred]
  (let [deadline       (+ (System/currentTimeMillis)
                          (* 1000 timeout-seconds))
        ;; before the request, since an algorithm that the platform lacks
        ;; throws
        digests        (into {} (for [a algorithms]
                                  [a (MessageDigest/getInstance a)]))
        [response _]   (send-following! request deadline allow-pred)
        status         (.statusCode ^HttpResponse response)
        url            (str (.uri ^HttpResponse response))
        update-digests (fn [^bytes buffer n]
                         (doseq [^MessageDigest d (vals digests)]
                           (.update d buffer 0 (int n))))]
    (with-open [^InputStream in (.body ^HttpResponse response)]
      (when (= 200 status)
        (within in deadline url #(pump in nil url update-digests))))
    (cond-> {:status status :url url}
      (= 200 status)
      (assoc :digests (update-vals digests #(.digest ^MessageDigest %))))))
