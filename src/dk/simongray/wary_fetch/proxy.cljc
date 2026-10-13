(ns dk.simongray.wary-fetch.proxy
  "A proxy on your own server for the requests of a web page, since most
  servers don't let a page of another site read their answers.

  The proxy fetches any public URL for whoever can reach it, so put it
  behind your login. Its answers come from other sites but have the
  origin of yours, so it refuses to be opened as a page, and no answer
  runs scripts when a browser opens it anyway."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.async :as async]
            [dk.simongray.wary-fetch.bytes :as bytes]
            [dk.simongray.wary-fetch.headers :as headers]
            #?(:clj [dk.simongray.wary-fetch.jvm :as jvm])
            #?(:clj [dk.simongray.wary-fetch.limits :as limits])
            [dk.simongray.wary-fetch.redirect :as redirect]
            [dk.simongray.wary-fetch.url :as url]
            [dk.simongray.whatwg-mime :as mime])
  #?(:clj (:import [java.io FilterInputStream InputStream]
                   [java.net.http HttpResponse]
                   [java.util.concurrent
                    ScheduledFuture ScheduledThreadPoolExecutor TimeUnit])))

(def defaults
  "The options of the proxy, which its own options replace one by one:

  - :allow-pred, the URLs that it fetches, those on the public internet
  - :send, the function that sends a request, http/send!
  - :forward, the request headers that go on to the other site
  - :expose, the response headers that come back to the page
  - :user-agent, the User-Agent of each request
  - :max-bytes, the most that a body can hold, 32 MB, since the proxy
    holds each body in memory

  Each request also has the :timeout-seconds of the options, if any. The
  page's own cookies and credentials never go on, unless :forward names
  them."
  {:allow-pred url/public?
   :send       http/send!
   :forward    #{"accept" "if-modified-since" "if-none-match" "range"}
   :expose     #{"age" "cache-control" "content-range" "content-type" "etag"
                 "expires" "last-modified" "link" "retry-after"}
   :user-agent http/user-agent
   :max-bytes  (* 32 1024 1024)})

(def ^:no-doc via
  "The Via of each request that the proxy sends, as RFC 9110, section
  7.6.3, has it, so that it can tell a request of its own and refuse to
  fetch itself."
  "1.1 wary-fetch")

(def ^:no-doc permanent-header
  "The response header that names the URL that redirects moved the
  request to for good, since HTTP has no header for it. The name has no
  X-, as RFC 6648 asks, and is one of this library's own."
  "Wary-Fetch-Permanent-Location")

(def ^:no-doc location-header
  "The response header that names the Location of a redirect that the
  client didn't follow. A browser would follow a Location itself, to a
  site that its page can't read, so it goes under a name of its own."
  "Wary-Fetch-Location")

;; RFC 9209, section 2.3
(def ^:no-doc errors
  "The error of the Proxy-Status of RFC 9209, and the status of the
  answer, for each type of exception of a request that the proxy answers
  with. Any other failure is ::failed."
  {::http/refused   ["http_request_denied" 403]
   ::http/timeout   ["http_response_timeout" 504]
   ::http/too-large ["http_response_body_size" 502]
   ::failed         ["destination_unavailable" 503]})

(def ^:no-doc safety
  "The headers of every answer of the proxy, which comes from another site
  but has the origin of yours: a browser that opens it runs no scripts,
  guesses no other type, and lets no other site embed it."
  {"Content-Security-Policy"      "default-src 'none'; sandbox"
   "X-Content-Type-Options"       "nosniff"
   "Cross-Origin-Resource-Policy" "same-origin"})

(defn- target
  "The parameters of the `ring-request` by name: the url to fetch, and as,
  which is bytes for a body of bytes."
  [ring-request]
  (url/query-params (:query-string ring-request)))

(defn- header-names
  "The headers of the `ring-request` by their names in lower case, since
  not every adapter gives them so."
  [ring-request]
  (update-keys (:headers ring-request) str/lower-case))

;; RFC 9209, section 2, with the details as a string of RFC 8941, section 3.3.3
(defn- answer
  "The answer of the proxy with `status` when it doesn't send the request:
  the `error` of RFC 9209, and the `details` as text."
  [status error details]
  (let [quoted       (-> details
                         (str/replace #"[\\\"]" #(str "\\" %))
                         (str/replace #"[^\x20-\x7E]" "?"))
        proxy-status (str "wary-fetch; error=" error
                          "; details=\"" quoted "\"")]
    {:status  status
     :headers (assoc safety
                     "Content-Type" "text/plain; charset=utf-8"
                     "Proxy-Status" proxy-status)
     :body    (bytes/encode details)}))

;; Fetch Metadata Request Headers, W3C Working Draft of 2021-03-29
(defn refusal
  "The Ring response that refuses `ring-request`, or nil when the proxy
  sends it on. The proxy refuses:

  - a method other than GET and HEAD
  - a request that it sent itself, as its Via tells
  - a page that opens it as a document, or a page of another site, as the
    Sec-Fetch-Mode and Sec-Fetch-Site of the browser tell
  - a request without an http or https URL as its url parameter"
  [{:keys [request-method] :as ring-request}]
  (let [headers (header-names ring-request)]
    (cond
      ;; TODO: consider sending writes to hosts that the operator names, none
      ;; by default, e.g. so that a page can write to a WebDAV server. That
      ;; takes the methods of writes, the request body within :max-bytes,
      ;; Authorization to those hosts only, Sec-Fetch-Site same-origin for
      ;; every write, and the host check in :allow-pred, so that each redirect
      ;; meets it. Hosts that users choose would make the proxy a relay of
      ;; writes for every account.
      (not (#{:get :head} request-method))
      (-> (answer 405 "http_request_denied" "The proxy answers GET and HEAD.")
          (assoc-in [:headers "Allow"] "GET, HEAD"))

      (str/includes? (str (get headers "via")) via)
      (answer 502 "proxy_loop_detected" "The proxy doesn't fetch itself.")

      (or (= "navigate" (get headers "sec-fetch-mode"))
          (= "cross-site" (get headers "sec-fetch-site")))
      (answer 403 "http_request_denied"
              (str "The proxy answers the requests of the pages of its own"
                   " site, and isn't a page itself."))

      (not (url/web-url? (get (target ring-request) "url")))
      (answer 400 "http_request_error"
              "The url parameter names no http or https URL."))))

(defn upstream-request
  "The request that the proxy sends for `ring-request`, with `opts` over
  defaults, or nil when the url parameter names no http or https URL.

  The request has the headers of the :forward of `opts`, the Via of the
  proxy, and the range of the Range header, so that no more is read than
  it asks for. Its body is bytes when the as parameter is bytes, and else
  text."
  ([ring-request]
   (upstream-request ring-request {}))
  ([{:keys [request-method] :as ring-request} opts]
   (let [{:keys [forward] :as opts} (merge defaults opts)
         {:strs [url as]} (target ring-request)
         headers          (header-names ring-request)
         [asked :as asks] (when (contains? forward "range")
                            (headers/byte-ranges (get headers "range")))
         forwarded        (select-keys headers (disj forward "range"))]
     (when (url/web-url? url)
       (cond-> (http/with-options {:method  request-method
                                   :url     url
                                   :headers (assoc forwarded "Via" via)}
                                  opts)
         (= "bytes" as)     (assoc :as :bytes)
         (= 1 (count asks)) (assoc :range asked))))))

;; MIME Sniffing, section 4.6, the groups of JSON, XML and JavaScript
(defn- text-type?
  "Whether the MIME type `m` holds text: a type of text, JSON, XML or
  JavaScript."
  [m]
  (or (= "text" (:type m))
      (mime/json? m)
      (mime/xml? m)
      (mime/javascript? m)))

(defn ring-response
  "The Ring response that gives the page the `response` of its request,
  with the headers of the :expose of `opts`, or else of defaults.

  A text body goes as UTF-8, since a page reads the answers of fetch in
  it, and a type of text says so. Any other type stays as it came, so ask
  for bytes for a body that isn't text. The URL that answered, after
  redirects, is the Content-Location (RFC 9110, section 8.7). The URL that
  permanent redirects led to is the Wary-Fetch-Permanent-Location.

  A redirect that the client didn't follow, e.g. from https to http, keeps
  its status, and its Location goes as the Wary-Fetch-Location."
  ([response]
   (ring-response response {}))
  ([{:keys [status url permanent-url headers body] :as response}
    {:keys [expose] :or {expose (:expose defaults)} :as opts}]
   (let [type         (when (contains? expose "content-type")
                        (mime/extract (get headers "content-type")))
         content-type (mime/serialize
                       (cond-> type
                         (and (string? body) (text-type? type))
                         (assoc-in [:parameters "charset"] "utf-8")))
         location     (when (redirect/statuses status)
                        (some->> (headers/header headers "location")
                                 (url/absolute-url url)))
         exposed      (for [h (disj expose "content-type")
                            :let [v (headers/joined headers h)]
                            :when v]
                        [h v])]
     {:status  status
      ;; under this name, since a Ring adapter such as Pedestal's adds its own
      ;; Content-Type to a body of bytes otherwise
      :headers (cond-> (into safety exposed)
                 type          (assoc "Content-Type" content-type)
                 url           (assoc "Content-Location" url)
                 permanent-url (assoc permanent-header permanent-url)
                 location      (assoc location-header location))
      :body    (if (string? body) (bytes/encode body) body)})))

(defn error-response
  "The Ring response for `e`, the exception of the :send of the proxy, with
  the Proxy-Status of RFC 9209, from which the page makes the same
  exception again."
  [e]
  (let [type           (:type (ex-data e))
        kind           (cond
                         (http/timeout? e)       ::http/timeout
                         (contains? errors type) type
                         :else                   ::failed)
        [error status] (errors kind)]
    (answer status error (async/explain e))))

#?(:clj
   (defn handler
     "A Ring handler that sends each GET and HEAD request on to the URL of
     its url parameter, with `opts` over defaults, and answers with what
     came back.

     Mount it with any router, e.g. Pedestal, ring-jetty or http-kit,
     behind your login."
     ([]
      (handler {}))
     ([opts]
      {:pre [(map? opts)]}
      (let [opts (merge defaults opts)]
        (fn [ring-request]
          (or (refusal ring-request)
              (try
                (let [request (upstream-request ring-request opts)]
                  (ring-response ((:send opts) request) opts))
                (catch Exception e
                  (error-response e)))))))))

(def stream-defaults
  "The options of the streaming proxy, which its own options replace one by
  one: those of defaults, but for these, since it holds no body in memory:

  - :max-bytes, the most that one answer passes on, 2 GB
  - :stream-seconds, how long one answer takes at most to pass on, an hour
  - :expose, also the Accept-Ranges and the Content-Length

  The :send of defaults doesn't count, since the JDK sends each request."
  (-> defaults
      (dissoc :send)
      (assoc :max-bytes      (* 2 1024 1024 1024)
             :stream-seconds 3600)
      (update :expose conj "accept-ranges" "content-length")))

#?(:clj
   (defn- bounded
     "The body `in` of an answer, which ends after `max-bytes` and closes at
     the instant `deadline`, so that a slow or an endless answer holds no
     thread for longer."
     ^InputStream [^InputStream in max-bytes deadline]
     (let [left ^longs (long-array [max-bytes])
           wait (long (max 0 (- deadline (System/currentTimeMillis))))
           task (.schedule ^ScheduledThreadPoolExecutor @jvm/watchdog
                           ^Runnable (fn [] (.close in))
                           wait
                           TimeUnit/MILLISECONDS)]
       (proxy [FilterInputStream] [in]
         (read
           ([]
            (if (pos? (aget left 0))
              (let [b (.read in)]
                (when-not (neg? b) (aset left 0 (dec (aget left 0))))
                b)
              -1))
           ([^bytes buffer]
            (.read ^InputStream this buffer 0 (alength buffer)))
           ([^bytes buffer offset length]
            (if (pos? (aget left 0))
              (let [n (.read in buffer (int offset)
                             (int (min length (aget left 0))))]
                (when (pos? n) (aset left 0 (- (aget left 0) n)))
                n)
              -1)))
         (close []
           (.cancel ^ScheduledFuture task false)
           (.close in))))))

#?(:clj
   (defn- stream-response
     "The Ring response that passes on the `response` of the JDK to the
     prepared `request` after the redirects `chain`, its body as it arrives,
     with the headers of the :expose of `opts`.

     An answer whose Content-Length is over the :max-bytes of `opts` is
     refused, before any of its body passes on."
     [^HttpResponse response chain {:keys [method] :as request}
      {:keys [expose max-bytes stream-seconds] :as opts}]
     (let [status   (.statusCode response)
           url      (str (.uri response))
           in       ^InputStream (.body response)
           headers  (jvm/headers-of response)
           length   (some-> (headers/header headers "content-length")
                            parse-long)
           moved    (redirect/permanent-url chain url)
           location (when (redirect/statuses status)
                      (some->> (headers/header headers "location")
                               (url/absolute-url url)))
           exposed  (for [h    (disj expose "content-type" "content-length")
                          :let [v (headers/joined headers h)]
                          :when v]
                      [h v])
           type     (headers/header headers "content-type")
           deadline (+ (System/currentTimeMillis) (* 1000 stream-seconds))]
       (if (and length max-bytes (< max-bytes length))
         (do (.close in)
             (error-response (limits/too-large url max-bytes)))
         {:status  status
          ;; under these names, since a Ring adapter adds its own otherwise
          :headers (cond-> (assoc (into safety exposed) "Content-Location" url)
                     type     (assoc "Content-Type" type)
                     length   (assoc "Content-Length" (str length))
                     moved    (assoc permanent-header moved)
                     location (assoc location-header location))
          :body    (if (= :head method)
                     (do (.close in) nil)
                     (bounded in (or max-bytes Long/MAX_VALUE) deadline))}))))

;; RFC 9110, section 14: a request for a range goes on as it is, and its
;; answer of 206 comes back with its Content-Range
#?(:clj
   (defn stream-handler
     "A Ring handler that sends each GET and HEAD request on to the URL of
     its url parameter, as handler does, with `opts` over stream-defaults,
     but passes on the body as it arrives rather than holding it, e.g. for
     an episode that a page saves.

     The body goes on as it came, without gzip undone or text decoded, and
     a Range goes on, so that a page can resume a download. A redirect is
     followed by the proxy, which asks :allow-pred about each URL."
     ([]
      (stream-handler {}))
     ([opts]
      {:pre [(map? opts)]}
      (let [opts (merge stream-defaults opts)]
        (fn [ring-request]
          (or (refusal ring-request)
              (try
                (let [request  (-> (upstream-request ring-request opts)
                                   (assoc :as :bytes)
                                   (assoc-in [:headers "Accept-Encoding"]
                                             "identity")
                                   http/prepared)
                      timeout  (:timeout-seconds opts 30)
                      deadline (+ (System/currentTimeMillis) (* 1000 timeout))
                      [response chain]
                      (jvm/send-following! request deadline (:allow-pred opts))]
                  (stream-response response chain request opts))
                (catch Exception e
                  (error-response e)))))))))

(defn proxied
  "The `request` sent through the proxy at `base`: to `base`, with the URL
  of the request as the url parameter, and without its :allow-pred, which the
  proxy applies itself."
  [base {:keys [url as] :as request}]
  (let [as-param (when (= :bytes as) "bytes")]
    (-> request
        (dissoc :allow-pred)
        (assoc :url (url/with-query base {:url url :as as-param})))))

(defn- failure
  "The exception for `request`, which the proxy failed to send, as the
  `proxy-status` of its answer tells. Nil when it tells of no failure of
  this proxy."
  [request proxy-status]
  (when-let [[_ error] (re-find #"wary-fetch; error=([a-z_]+)" proxy-status)]
    (let [quoted  #"details=\"((?:[^\"\\]|\\.)*)\""
          details (some-> (re-find quoted proxy-status)
                          second
                          (str/replace #"\\(.)" "$1"))
          kind    (or (some (fn [[kind [e]]] (when (= e error) kind)) errors)
                      ::failed)
          message (or details
                      (str "The proxy failed to send the request to "
                           (:url request)))]
      (ex-info message {:type kind :url (:url request)}))))

(defn unproxied
  "The `response` of the proxy to `request`, as a direct request would
  give it.

  The response has the :url that answered, after redirects, the
  :permanent-url that permanent redirects led to, and the Location of a
  redirect that wasn't followed. When the proxy failed to send the
  request, this throws the exception that a direct request would have
  thrown, with the same :type."
  [request {:keys [headers] :as response}]
  (if-let [e (some->> (headers/joined headers "proxy-status")
                      (failure request))]
    (throw e)
    (let [moved    (headers/header headers permanent-header)
          location (headers/header headers location-header)
          url      (or (headers/header headers "content-location")
                       (:url request))]
      (cond-> (assoc response :url url)
        moved    (assoc :permanent-url moved)
        location (assoc-in [:headers "location"] [location])))))

(defn sender
  "A function to give as :send, which sends each request through the
  proxy at `base`, with `opts`, and gives what a direct request would.

  The `base` is the URL of the route of the proxy, e.g. /proxy in a page.
  Each request goes with the :send of `opts`, http/send! by default. A
  request whose URL isn't http or https, e.g. a WebSocket that a page opens
  by itself, goes to that :send as it is."
  ([base]
   (sender base {}))
  ([base {:keys [send] :or {send http/send!} :as opts}]
   (fn [request]
     (if (url/web-url? (:url request))
       (async/then (send (proxied base request)) #(unproxied request %))
       (send request)))))

#?(:clj
   (comment
     (def server (handler))
     (def feed-url "https://feeds.podcastindex.org/pc20.xml")
     (def response
       (server {:request-method :get
                :query-string   (url/query-string {:url feed-url})
                :headers        {"accept" "application/rss+xml"}}))
     (select-keys response [:status :headers])

     ;; a page that opens the proxy is refused
     (:status (server {:request-method :get
                       :query-string   "url=https%3A%2F%2Fexample.org%2F"
                       :headers        {"sec-fetch-mode" "navigate"}}))
     #_.))
