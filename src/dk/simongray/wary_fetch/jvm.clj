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
                    IOException OutputStream]
           [java.net ConnectException InetAddress URI UnknownHostException]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest
                          HttpRequest$Builder HttpRequest$BodyPublishers
                          HttpResponse HttpResponse$BodyHandlers
                          HttpTimeoutException]
           [java.nio.channels UnresolvedAddressException]
           [java.security MessageDigest]
           [java.time Duration]
           [java.util Arrays]
           [java.util.concurrent CompletableFuture ExecutionException
                                 ScheduledFuture ScheduledThreadPoolExecutor
                                 ThreadFactory TimeoutException TimeUnit]
           [java.util.zip GZIPInputStream]
           [javax.net.ssl SSLException]))

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
  "Read `in` from `url` in pieces to its end, or to `limit` bytes when it's
  given, give each piece to `f`, and throw ::too-large once more than
  `max-bytes` have arrived. It gives how many bytes it read.

  The `f` takes the buffer and the length of the piece. With nil
  `max-bytes`, it reads everything."
  ([in max-bytes url f]
   (pump in nil max-bytes url f))
  ([^InputStream in limit max-bytes url f]
   (let [size   65536
         buffer (byte-array size)]
     (loop [total 0]
       (let [want (if limit (min size (- limit total)) size)
             n    (if (pos? want) (.read in buffer 0 (int want)) -1)]
         (if (neg? n)
           total
           (let [total (+ total n)]
             (when (and max-bytes (> total max-bytes))
               (throw (limits/too-large url max-bytes)))
             (f buffer n)
             (recur total))))))))

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

(def ^:no-doc signal-ms
  "How often the :signal of a request is checked while the request waits,
  in milliseconds."
  50)

(defn within
  "The result of `f`, which reads `in` from `url`, but only until the
  instant `deadline` in epoch milliseconds, or until the `signal` of the
  request says to stop.

  At the deadline, the stream is closed and ::timeout thrown, so that a
  server that sends its body slowly holds no thread for longer, and when
  the signal says to stop, ::aborted."
  [^InputStream in deadline url signal f]
  (let [late?    (atom false)
        stopped? (atom false)
        wait-ms  (long (max 0 (- deadline (System/currentTimeMillis))))
        ^ScheduledThreadPoolExecutor dog @watchdog
        task     (.schedule dog
                            ^Runnable (fn [] (reset! late? true) (.close in))
                            wait-ms
                            TimeUnit/MILLISECONDS)
        watch    (when signal
                   (.scheduleWithFixedDelay
                    dog
                    ^Runnable (fn []
                                (when (limits/stopped? signal)
                                  (reset! stopped? true)
                                  (.close in)))
                    (long signal-ms)
                    (long signal-ms)
                    TimeUnit/MILLISECONDS))
        failure  #(cond @stopped? (limits/aborted url)
                        @late?    (limits/late url))]
    (try
      (let [result (f)]
        ;; a stream closed under a read can end as if the body had ended
        (if-let [e (failure)] (throw e) result))
      (catch IOException e
        (throw (or (failure) e)))
      (finally
        (.cancel ^ScheduledFuture task false)
        (some-> ^ScheduledFuture watch (.cancel false))))))

(defn- read-within
  "The bytes of `in` from `url` within `max-bytes`, but only until the
  `deadline`, or until the `signal` says to stop."
  ^bytes [^InputStream in deadline max-bytes url signal]
  (within in deadline url signal #(read-bounded in max-bytes url)))

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

  With nil `max-bytes`, there's no limit. The body is decoded in the
  charset of the Content-Type, or else the one that an HTML page or an
  XML document names."
  ([headers body]
   (decode headers body nil))
  ([headers ^bytes body max-bytes]
   (encoding/decode (inflated headers body max-bytes) headers)))

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

(defn- unresolved?
  "Whether the host of `uri` is a name that doesn't resolve."
  [^URI uri]
  (try
    (some-> (.getHost uri) InetAddress/getAllByName)
    false
    (catch UnknownHostException _
      true)))

(defn- connect-failure
  "The exception of the request to `url` that failed with `e` before an
  answer: typed when its causes say that the name didn't resolve, that the
  host couldn't be reached or that the secure connection failed, and else
  `e`. A timeout stays as it is."
  [url ^Throwable e]
  (let [causes (take-while some? (iterate ex-cause e))
        any    (fn [& classes]
                 (some (fn [x] (when (some #(instance? % x) classes) x))
                       causes))]
    (cond
      (instance? HttpTimeoutException e)
      e

      (any UnresolvedAddressException UnknownHostException)
      (limits/unresolved url e)

      (any SSLException)
      (limits/tls-failure url e (ex-message (any SSLException)))

      (any ConnectException)
      (limits/unreachable url e)

      :else
      e)))

(defn- exchange!
  "The response of the JDK to the request `outgoing` to `url`, with its
  body as a stream, or ::aborted once the `signal` of the request says to
  stop while it waits."
  ^HttpResponse [^HttpRequest outgoing url signal]
  (let [handler (HttpResponse$BodyHandlers/ofInputStream)]
    (if-not signal
      (.send ^HttpClient @client outgoing handler)
      (let [^CompletableFuture answer (.sendAsync ^HttpClient @client
                                                  outgoing
                                                  handler)]
        (loop []
          (when (limits/stopped? signal)
            (.cancel answer true)
            (throw (limits/aborted url)))
          (let [response (try
                           (.get answer (long signal-ms) TimeUnit/MILLISECONDS)
                           (catch TimeoutException _
                             nil)
                           (catch ExecutionException e
                             (throw (or (ex-cause e) e))))]
            (or response (recur))))))))

(defn send-following!
  "Send the `request`, follow its redirects until the instant `deadline`
  in epoch milliseconds, and check each URL with `allow-pred`.

  It gives the last response and the redirects before it, each as a
  vector of its status, URL and headers. It follows the redirects itself,
  since the JDK sends credentials on to another origin. A URL that
  `allow-pred` refuses throws ::refused, or ::unresolved when its host is
  a name that doesn't resolve, as url/public? refuses such a name. A
  header value that the JDK would refuse throws ::invalid-header, since
  the JDK's own exception shows the value. Once the :signal of the request
  says to stop, it throws ::aborted."
  [request deadline allow-pred]
  (loop [request (update request :url url/legal-url)
         chain   []]
    (let [{:keys [url method headers body signal]} request
          uri  (URI. url)
          left (- deadline (System/currentTimeMillis))]
      (when (limits/stopped? signal)
        (throw (limits/aborted url)))
      (when-not (pos? left)
        (throw (HttpTimeoutException. (str "request timed out: " uri))))
      (when-not (allow-pred url)
        (throw (if (unresolved? uri)
                 (limits/unresolved url nil)
                 (limits/refusal url))))
      (when (limits/unsendable-header headers)
        (throw (limits/invalid-header url headers)))
      (let [outgoing (java-request uri method headers body left)
            response (try
                       (exchange! outgoing url signal)
                       (catch IOException e
                         (throw (connect-failure url e))))
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

(defn- cut-bytes
  "The bytes of the body `in` of an answer from `url` with `headers`, with
  gzip undone, read until `deadline` or the `signal`, but no further than
  `max-bytes`, as response-bytes gives them, :truncated? when there were
  more."
  [^InputStream in headers url max-bytes deadline signal]
  (let [out (ByteArrayOutputStream.)]
    ;; one byte more, to tell a body cut short from one that just fits
    (within in deadline url signal
            #(pump (if (headers/gzipped? headers) (GZIPInputStream. in) in)
                   (inc (long max-bytes)) nil url
                   (fn [^bytes buffer n] (.write out buffer 0 (int n)))))
    (let [b    (.toByteArray out)
          cut? (> (alength b) (long max-bytes))]
      {:bytes      (if cut? (Arrays/copyOf b (int max-bytes)) b)
       :inflated?  true
       :truncated? cut?})))

(defn- response-bytes
  "The bytes of the body of `response` to `request`, read until `deadline`,
  no more than the range of the request needs, as a map of the :bytes, and
  :inflated? when gzip is undone already. With the :truncate? of the
  request, a body larger than its :max-bytes is cut, and :truncated?."
  [^HttpResponse response
   {:keys [method url range max-bytes truncate? signal] :as request}
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
        (= :head method)
        {:bytes (byte-array 0)}

        n
        {:bytes (within in deadline url signal #(.readNBytes in (int n)))}

        (and truncate? max-bytes)
        (cut-bytes in headers url max-bytes deadline signal)

        :else
        {:bytes (read-within in deadline max-bytes url signal)}))))

(defn- answer
  "The response map for the `response` of the JDK to `request`, with the
  `body` as response-bytes gives it, which came after the redirects
  `chain`."
  [^HttpResponse response {:keys [^bytes bytes inflated? truncated?] :as body}
   chain {:keys [as max-bytes] :as request}]
  (let [status   (.statusCode response)
        headers  (headers-of response)
        final    (str (.uri response))
        moved    (redirect/permanent-url chain final)
        received (headers/content-range
                  (headers/header headers "content-range"))
        content  (when (and (not= 304 status) (pos? (alength bytes)))
                   (let [b (if inflated?
                             bytes
                             (inflated headers bytes max-bytes))]
                     (if (= :bytes as)
                       b
                       (encoding/decode b headers))))]
    (cond-> {:status  status
             :url     final
             :headers headers
             :body    content}
      moved       (assoc :permanent-url moved)
      (seq chain) (assoc :redirects chain)
      received    (assoc :content-range received)
      truncated?  (assoc :truncated? true))))

(defn- sink-fn
  "A function of a buffer and the length of a piece in it that gives the
  piece to `sink`: an OutputStream, or a function of the bytes of each
  piece."
  [sink]
  (if (instance? OutputStream sink)
    (fn [^bytes buffer n]
      (.write ^OutputStream sink buffer 0 (int n)))
    (fn [^bytes buffer n]
      (sink (Arrays/copyOf buffer (int n))))))

(defn- sunk!
  "Give the body of `response` to `request` to its :sink in pieces until
  `deadline`, with gzip undone, no more than its range needs and within
  its :max-bytes, and give how many bytes the sink got."
  [^HttpResponse response
   {:keys [url range max-bytes sink signal] :as request}
   deadline]
  (let [headers  (headers-of response)
        gzipped? (headers/gzipped? headers)
        start    (:start (headers/content-range
                          (headers/header headers "content-range")))
        n        (limits/prefix-length (.statusCode response) start gzipped?
                                       range max-bytes)]
    (with-open [^InputStream in (.body response)]
      (within in deadline url signal
              #(pump (if gzipped? (GZIPInputStream. in) in)
                     n max-bytes url (sink-fn sink))))))

(defn send!
  "Send the `request` within `timeout-seconds`, to URLs that `allow-pred`
  takes, and give the response. The body of a success goes to the :sink of
  the request, when it has one."
  [{:keys [method sink] :as request} timeout-seconds allow-pred]
  (let [deadline         (+ (System/currentTimeMillis)
                            (* 1000 timeout-seconds))
        [response chain] (send-following! request deadline allow-pred)
        status           (.statusCode ^HttpResponse response)]
    (if (and sink (not= :head method) (<= 200 status 299))
      (let [received (sunk! response request deadline)]
        (assoc (answer response {:bytes (byte-array 0)} chain request)
               :received received))
      (answer response (response-bytes response request deadline) chain
              request))))

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
        (within in deadline url (:signal request)
                #(pump in nil url update-digests))))
    (cond-> {:status status :url url}
      (= 200 status)
      (assoc :digests (update-vals digests #(.digest ^MessageDigest %))))))
