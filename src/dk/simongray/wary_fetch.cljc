(ns dk.simongray.wary-fetch
  "Requests and responses as maps, and the functions that send them.

  The client is java.net.http on the JVM, node:http in Node and fetch in a
  browser. A request moves no more bytes than its purpose needs."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.async :as async]
            #?(:cljs [dk.simongray.wary-fetch.browser :as browser])
            [dk.simongray.wary-fetch.headers :as headers]
            [dk.simongray.wary-fetch.json :as json]
            #?(:clj [dk.simongray.wary-fetch.jvm :as jvm])
            [dk.simongray.wary-fetch.limits :as limits]
            #?(:cljs [dk.simongray.wary-fetch.node :as node])
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.wary-fetch.url :as url]
            [dk.simongray.webcrypto :as webcrypto]
            [dk.simongray.webcrypto.bytes :as bytes]
            [dk.simongray.whatwg-mime :as mime])
  #?(:clj (:import [java.net.http HttpTimeoutException])))

;; RFC 9110, section 10.1.5: a product and its version. The bot rules of
;; hosts behind Cloudflare refuse a product without one, and some refuse a
;; comment with a URL, as of 2026-10-11. The jar names its version in place
;; of dev.
(def user-agent
  "The User-Agent of a request outside a browser, unless the request names
  another."
  "wary-fetch/dev")

(defn- base64
  "The UTF-8 bytes of `s` in base64."
  [s]
  (bytes/base64 (bytes/encode s)))

;; RFC 7617, section 2 (Basic); RFC 6750, section 2.1 (Bearer)
(defn authorization
  "The Authorization header for `credentials`, or nil for no credentials.

  The `credentials` are a map, or a function of no arguments that returns
  one. The map has :username and :password for basic authentication, or
  :token for a bearer token. Hide the password or the token with
  secret/hide, so that a printed map of credentials shows #<secret>.

  The header holds the password or the token, so it's hidden too, and a
  printed request map doesn't show it."
  [credentials]
  (let [m        (if (fn? credentials) (credentials) credentials)
        username (:username m)
        password (secret/reveal (:password m))
        token    (secret/reveal (:token m))]
    (cond
      token
      (secret/hide (str "Bearer " token))

      username
      (secret/hide (str "Basic " (base64 (str username ":" password))))

      :else
      nil)))

(defn revealed
  "The `request` with the texts of its hidden headers and body, as a
  server needs them.

  The Authorization header that authorization makes is hidden, and so is
  the body of form-request and multipart-request. Another client needs
  their texts, but a revealed request shows its credentials when printed."
  [{:keys [headers] :as request}]
  (cond-> request
    headers                   (update :headers update-vals secret/reveal)
    (contains? request :body) (update :body secret/reveal)))

#?(:cljs
   (def ^:no-doc node?
     "Whether send! sends with node:http, as it does in Node, rather than
     with fetch."
     (and (not browser/available?) node/available?)))

(defn- request-headers
  "The `headers` of a request for the bytes `range`, with a Range header
  and the User-Agent `agent` (unless they name another or a browser sends
  its own).

  On the JVM and in Node, a request for a range asks for bytes that aren't
  gzipped, so that the offsets are those of the file."
  [headers range agent]
  (cond-> (merge #?(:clj  {"User-Agent" agent}
                    :cljs (when-not browser/available? {"User-Agent" agent}))
                 headers)
    range (assoc "Range" (str "bytes=" (first range) "-" (second range)))
    #?@(:clj  [range (assoc "Accept-Encoding" "identity")]
        :cljs [(and range node?) (assoc "Accept-Encoding" "identity")])))

(defn ^:no-doc prepared
  "The `request` as a client sends it, with:

  - its method as a keyword in lower case
  - the headers that request-headers adds
  - its :query-params in its URL
  - the default :max-bytes unless it has its own
  - its hidden headers and body revealed"
  [{:keys [method headers query-params] :as request}]
  (let [agent (or (:user-agent request) user-agent)]
    (-> (merge {:max-bytes limits/default-max-bytes} request)
        (assoc :method  (some-> method name str/lower-case keyword)
               :headers (request-headers headers (:range request) agent))
        (cond-> query-params
          (update :url url/with-query query-params))
        (dissoc :query-params)
        revealed)))

(defn send!
  "Send the `request` and return the response (in ClojureScript a promise
  of it).

  The request is a map of :method, :url, :headers and :body. It's a GET
  unless :method names another. The body is text or bytes. You can hide
  the header values and the body with secret/hide. A request can also
  have these keys:

  - :query-params, a map of parameters to add after the URL's own query,
    as url/with-query adds them
  - :as, either :bytes for a body of bytes rather than text, or :json for
    the value of a JSON body as json/read-json returns it (nil when it
    isn't JSON)
  - :timeout-seconds, the limit of the whole exchange (30 by default)
  - :max-bytes, the limit of the body (100 MB by default or nil for none)
  - :truncate?, true to cut a body that's larger than :max-bytes rather
    than throw (e.g. to read the start of a large page), which marks the
    response :truncated?
  - :range, the first and the last byte to ask for (a nil last byte asks
    for the rest)
  - :allow-pred, a predicate of each URL, e.g. url/public?
  - :user-agent, the User-Agent outside a browser unless the :headers
    name one
  - :max-redirects, how many redirects to follow (5 by default, and 0 for
    none)
  - :sink, to take the body of a success in pieces rather than hold it,
    e.g. to write a large file. It's a function of the bytes of each
    piece (which in ClojureScript can return a promise to wait for), an
    OutputStream on the JVM or a writable stream in Node.
  - :signal, to stop the exchange: an AbortSignal in ClojureScript, or on
    the JVM an atom, a promise or another reference that turns truthy
  - :prepare-fn, a function of the request of each hop (the first and
    each redirect) that returns the request to send, or a future or a
    promise of it, e.g. to sign each hop for its URL. It gets the request
    as the client sends it: with the User-Agent, with the query in its URL
    and with the headers and body revealed. A browser follows its own
    redirects, so there it gets the first hop alone.

  The response is a map of :status, the final :url, :headers and :body.
  The body has gzip undone and text decoded by its charset. A 206 also
  has the :content-range as a map of :start, :end and :total. In a
  browser, :filtered? marks a response whose headers CORS hides. With a
  :sink, a response with a status from 200 to 299 has no :body but the
  number of bytes that the sink :received, within the same limits.

  Outside a browser it follows the redirects itself and checks each URL
  with :allow-pred. It never follows one from https to http, and
  credentials stay behind when a redirect leaves the origin. The response
  then lists the :redirects and has a :permanent-url if the first ones
  were permanent. A browser follows its own redirects. With
  :max-redirects 0, it returns a redirect as a response with the status 0.

  When it fails, it throws one of these (in ClojureScript it rejects with
  one):

  - ::timeout or ::too-large past a limit
  - ::refused when :allow-pred refuses a URL
  - ::aborted once the :signal says to stop. On the JVM the library
    checks it every few hundredths of a second, and the Capacitor and
    Electron senders check it only before they send.
  - ::invalid-header for a header value with a character that HTTP
    doesn't allow (e.g. a line break). It names the header but leaves out
    the value.
  - ::invalid-body for a body that's a collection, e.g. a map meant for
    json-request
  - ::unresolved, ::unreachable or ::tls for a request that reaches no
    server, when the host's name doesn't resolve, the host can't be
    reached or the secure connection fails (e.g. over an expired
    certificate)

  A browser doesn't report why a request reached no server. When it
  reports the device as offline, the type is ::unreachable."
  [{:keys [timeout-seconds allow-pred as]
    :or   {timeout-seconds 30 allow-pred (constantly true)}
    :as   request}]
  (let [request (prepared request)
        sent    #?(:clj  (jvm/send! request timeout-seconds allow-pred)
                   :cljs (if node?
                           (node/send! request timeout-seconds allow-pred)
                           (browser/send! request timeout-seconds
                                          allow-pred)))]
    (if (= :json as)
      (async/then sent #(update % :body json/read-json))
      sent)))

(defn send-public!
  "Send the `request`, but only to a host on the public internet. On the
  JVM and in Node, the host of each redirect has to be public too.

  It sends no request to the app's own device or network, and throws
  ::refused instead (in ClojureScript it rejects with that)."
  [request]
  (send! (assoc request :allow-pred url/public?)))

(def ^:no-doc option-keys
  "The keys of a request that set how it's sent. The functions that send
  take them from their options."
  [:timeout-seconds :max-bytes :allow-pred :user-agent :max-redirects
   :truncate? :signal])

(defn with-options
  "The `request` with the options of send! that `opts` have and that
  aren't nil: :timeout-seconds, :max-bytes, :allow-pred, :user-agent,
  :max-redirects, :truncate? and :signal."
  [request opts]
  (into request (remove (comp nil? val)) (select-keys opts option-keys)))

;; HTML Standard, 4.10.21.8, application/x-www-form-urlencoded
(defn form-request
  "The request that posts the form `fields` to `url` with the `opts`
  below, e.g. to an OAuth token endpoint.

  The `fields` are a map or pairs of names and values, as
  url/query-string takes them. The body is hidden with secret/hide, since
  a form often holds a secret such as an OAuth code. The `opts` are the
  :headers to add and the options of send! that with-options takes, e.g.
  :max-redirects 0 to keep a form from following a redirect."
  ([url fields]
   (form-request url fields {}))
  ([url fields {:keys [headers] :as opts}]
   {:pre [(map? opts)]}
   (with-options
    {:method  :post
     :url     url
     :headers (merge {"Content-Type" "application/x-www-form-urlencoded"}
                     headers)
     :body    (secret/hide (url/query-string fields))}
    opts)))

;; RFC 8259, section 11
(defn json-request
  "The request that posts the value `x` to `url` as JSON, written by
  json/write-json, with the `opts` of form-request."
  ([url x]
   (json-request url x {}))
  ([url x {:keys [headers] :as opts}]
   {:pre [(map? opts)]}
   (with-options
    {:method  :post
     :url     url
     :headers (merge {"Content-Type" "application/json"} headers)
     :body    (json/write-json x)}
    opts)))

;; HTML Standard, 4.10.21.8: a browser escapes these in the names of a
;; multipart/form-data body
(defn- escaped
  "The name or file name `s` of a part, with its quotes and line breaks
  escaped."
  [s]
  (str/replace (str s) #"[\"\r\n]" {"\"" "%22" "\r" "%0D" "\n" "%0A"}))

(defn- part
  "The bytes of the part after the `boundary` for the field `k` and its
  value `v`, a text or a file as multipart-request takes them."
  [boundary [k v]]
  (let [file?   (map? v)
        content (if file? (:content v) (str v))]
    [(bytes/encode
      (str "--" boundary "\r\n"
           "Content-Disposition: form-data; name=\"" (escaped (name k)) "\""
           (when file?
             (str "; filename=\"" (escaped (or (:filename v) "blob")) "\"\r\n"
                  "Content-Type: " (or (:content-type v) "application/octet-stream")))
           "\r\n\r\n"))
     (if (string? content) (bytes/encode content) content)
     (bytes/encode "\r\n")]))

;; RFC 7578
(defn multipart-request
  "The request that posts the `parts` to `url` as multipart/form-data, with
  the `opts` of form-request, e.g. to upload a file.

  The `parts` are a map or pairs of names and values, like the fields of
  form-request. A value is a text or a file. A file is a map of its
  :content (bytes or a text), its :filename (blob by default, as in a
  browser) and its :content-type (application/octet-stream by default).

      (multipart-request \"https://example.com/media\"
                         [[\"file\" {:content      jpeg-bytes
                                   :filename     \"sunset.jpg\"
                                   :content-type \"image/jpeg\"}]])

  The body is bytes, hidden like the body of a form, and holds the files
  in memory."
  ([url parts]
   (multipart-request url parts {}))
  ([url parts {:keys [headers] :as opts}]
   {:pre [(map? opts)]}
   (let [boundary (str "wary-fetch-" (bytes/base64url (webcrypto/random-bytes 18)))]
     (with-options
      {:method  :post
       :url     url
       :headers (merge {"Content-Type" (str "multipart/form-data; boundary="
                                            boundary)}
                       headers)
       :body    (secret/hide
                 (apply bytes/concat
                        (concat (mapcat #(part boundary %) (remove (comp nil? second) parts))
                                [(bytes/encode (str "--" boundary "--\r\n"))])))}
      opts))))

(defn ^:no-doc public-by-default
  "The `opts` of a function that fetches, with url/public? as the
  :allow-pred unless they have another."
  [opts]
  (update opts :allow-pred #(or % url/public?)))

(defn aborted?
  "Whether the exception `e` of a request is ::aborted, since its :signal
  said to stop it."
  [e]
  (= ::aborted (:type (ex-data e))))

(defn timeout?
  "Whether the exception `e` of a request, or one of its causes, is a
  timeout: ::timeout, or the timeout of the platform's client."
  [e]
  (let [cause  #?(:clj ex-cause :cljs #(.-cause %))
        causes (take-while some? (iterate cause e))]
    (boolean (some #(or (= ::timeout (:type (ex-data %)))
                        #?(:clj  (instance? HttpTimeoutException %)
                           :cljs (= "TimeoutError" (.-name %))))
                   causes))))

(defn failure
  "The exception `e` of a request that failed, as data: its :reason, and
  the :type, :url and :status that it holds. For an exception with a
  :type, the reason is its own message, written for a person. For any
  other, it's the messages of the exception and its causes."
  [e]
  (let [data (ex-data e)]
    (into {:reason (if (:type data) (ex-message e) (async/explain e))}
          (select-keys data [:type :url :status]))))

(defn try-send!
  "Send the `request` with `send` (a function such as send!) and the
  `opts`, and return a map of the response as the :value, or of the
  exception as the :error when `send` throws. The `opts` are the options
  of send! that with-options puts in the request.

  The map comes in a future on the JVM and in a promise in ClojureScript,
  so several requests can be under way at once."
  ([send request]
   (try-send! send request {}))
  ([send request opts]
   (async/attempt #(send (with-options request opts)))))

(defn fetch-request
  "The request for the document at `url` with `opts`, the options of
  fetch!.

  A user name and a password in `url` are the credentials when `opts`
  have none, and the request goes to the URL without them. The request
  goes only to the public internet unless `opts` have an :allow-pred."
  ([url]
   (fetch-request url {}))
  ([url {:keys [etag last-modified credentials accept]
         :or   {accept "*/*"}
         :as   opts}]
   (let [[url in-url] (url/split-credentials url)
         auth         (authorization (or credentials in-url))]
     (with-options
      {:url     url
       :headers (cond-> {"Accept" accept}
                  etag          (assoc "If-None-Match" etag)
                  last-modified (assoc "If-Modified-Since" last-modified)
                  auth          (assoc "Authorization" auth))}
      (public-by-default opts)))))

(defn- now
  "The instant now, by the clock of the platform."
  []
  #?(:clj  (java.util.Date.)
     :cljs (js/Date.)))

(defn fetch!
  "GET the document at `url` with the `opts` below and return the response
  with the body as text (in ClojureScript a promise of it).

  The response also has :etag and :last-modified when the server sends
  them, and :not-before when the server says how long to wait.

  The `opts` are these:

  - :etag and :last-modified from an earlier response, sent as
    If-None-Match and If-Modified-Since, so that an unchanged document
    comes back as a 304 with no body
  - :credentials, as authorization takes them
  - :accept, the Accept header, */* by default
  - :timeout-seconds, :max-bytes, :max-redirects, :allow-pred,
    :user-agent, :truncate? and :signal, as send! takes them, but with
    url/public? as the :allow-pred by default
  - :max-wait-seconds, as headers/not-before takes it
  - :send, the function that sends the request, send! by default"
  ([url]
   (fetch! url {}))
  ([url {:keys [send] :or {send send!} :as opts}]
   (async/then (send (fetch-request url opts))
               (fn [{response-headers :headers :as response}]
                 (let [limits        (select-keys opts [:max-wait-seconds])
                       wait          (headers/not-before response-headers (now)
                                                         limits)
                       etag          (headers/header response-headers "etag")
                       last-modified (headers/header response-headers
                                                     "last-modified")]
                   (cond-> (assoc response
                                  :etag          etag
                                  :last-modified last-modified)
                     wait (assoc :not-before wait)))))))

#?(:clj
   (defn digest!
     "GET `url` with the `opts` below and return the digests of its body by
     the MessageDigest `algorithms`, e.g. SHA-384.

     The result has the :status and the :url. For a 200, it also has the
     :digests, which map each algorithm to the bytes of its digest. The
     body streams through the digests without being kept, so it can be a
     large media file.

     The `opts` are these:

     - :headers of the request
     - :allow-pred and :user-agent, as send! takes them, but with
       url/public? as the :allow-pred by default
     - :timeout-seconds, the limit of the whole exchange, 600 by default"
     ([url algorithms]
      (digest! url algorithms {}))
     ([url algorithms {:keys [headers timeout-seconds allow-pred]
                       :or   {timeout-seconds 600}
                       :as   opts}]
      (let [agent   (or (:user-agent opts) user-agent)
            request {:url     url
                     :method  :get
                     :headers (merge {"User-Agent"      agent
                                      "Accept-Encoding" "identity"}
                                     headers)}]
        (jvm/digests! (revealed request) algorithms timeout-seconds
                      (or allow-pred url/public?))))))

(defn- whole-size
  "The size of the file whose first `n` bytes came in a 200 to a request
  for the first `asked` bytes, by the answer's `content-length` and
  `content-encoding`.

  A 200 is the answer of a server that ignores ranges. The size is `n`
  when that's less than asked, since the file ended. Otherwise it's the
  content length, unless the content encoding changed the size."
  [n asked content-length content-encoding]
  (let [coding (some-> content-encoding str/trim str/lower-case)]
    (cond
      (< n asked)
      n

      (contains? #{nil "identity"} coding)
      (some-> content-length str/trim parse-long))))

(defn- failed
  "The exception for the answer to a request for the file at `url` whose
  `status` is a failure."
  [url status]
  (ex-info (str "The file answered " status ": " url)
           {:type ::failed :status status :url url}))

(defn fetcher
  "A function that fetches the bytes of the file at `url` by range, with
  the `opts` below.

  The function takes a start and an end (excluded) and returns a map of
  :bytes, :start and :total (in ClojureScript a promise of it). The total
  is the size of the file when the answer has it. A server that ignores
  ranges sends the file from its start, and the map then has :whole?. A
  range past the end of the file returns no bytes. Any other answer, e.g.
  a 404, throws ::failed with the :status.

  The `opts` are these:

  - :headers of the requests
  - :timeout-seconds, :max-bytes, :max-redirects, :allow-pred,
    :user-agent, :truncate? and :signal, as send! takes them, but with
    url/public? as the :allow-pred by default
  - :send, the function that sends each request, send! by default"
  ([url]
   (fetcher url {}))
  ([url {:keys [headers send] :or {send send!} :as opts}]
   (fn [start end]
     (let [request {:url     url
                    :headers headers
                    :as      :bytes
                    :range   [start (dec end)]}]
       (async/then
        (send (with-options request (public-by-default opts)))
        (fn [{:keys [status body content-range] :as response}]
          (let [body (or body (bytes/of []))
                h    #(headers/header (:headers response) %)]
            (case (long status)
              206 {:bytes body
                   :start (or (:start content-range) start)
                   :total (:total content-range)}
              200 {:bytes  body
                   :start  0
                   :whole? true
                   :total  (whole-size (bytes/length body) end
                                       (h "content-length")
                                       (h "content-encoding"))}
              ;; the body of a 416 is at most a page about the error
              416 {:bytes (bytes/of [])
                   :start start
                   :total (headers/complete-length (h "content-range"))}
              (throw (failed url status))))))))))

(defn facts
  "The facts about its file that the `response` has.

  The facts have these keys:

  - :status, :url and :permanent-url, as in the response
  - :redirects, each a map of its :status, the :url that answered with it
    and its :cors
  - :location, the Location of a redirect that wasn't followed
  - :type, the media type in lower case and without its parameters, as a
    browser parses the Content-Type
  - :length, the Content-Length as a number
  - :range, the Content-Range, as a map of :start, :end and :total
  - :encoding, :etag and :last-modified, from their headers
  - :cors, the origins that Access-Control-Allow-Origin allows to read
    the answer, with all its values joined as a browser joins them
  - :filtered?, as in the response"
  [response]
  (let [h        #(headers/header (:headers response) %)
        cors     #(headers/joined % "access-control-allow-origin")
        redirect (fn [[status url headers]]
                   (let [allowed (cors headers)]
                     (cond-> {:status status :url url}
                       allowed (assoc :cors allowed))))
        length   (some->> (h "content-length")
                          str/trim
                          (re-matches #"\d{1,15}")
                          parse-long)]
    (into {} (remove (comp nil? val))
          {:status        (:status response)
           :url           (:url response)
           :redirects     (not-empty (mapv redirect (:redirects response)))
           :permanent-url (:permanent-url response)
           :location      (h "location")
           :type          (mime/essence (mime/extract (get (:headers response) "content-type")))
           :length        length
           :range         (:content-range response)
           :encoding      (h "content-encoding")
           :etag          (h "etag")
           :last-modified (h "last-modified")
           :cors          (cors (:headers response))
           :filtered?     (:filtered? response)})))

(defn- sent-facts
  "The facts of the response in an `attempt` to send, or a map of the
  failure under :error when the request failed."
  [{:keys [value error] :as attempt}]
  (if error
    {:error (failure error)}
    (facts value)))

(defn probe!
  "Ask the server about the file at `url` with the `opts` below and return
  the facts (in ClojureScript a promise of them). It downloads no more
  than two bytes of the file.

  The result is a map of :head and :get with the facts of a HEAD request
  and of a request for the first two bytes. For a request that failed,
  the facts are its exception under :error, as failure returns it.

  The `opts` are these:

  - :headers of both requests, e.g. an Origin to learn about CORS
  - :timeout-seconds, :max-bytes, :max-redirects, :allow-pred,
    :user-agent, :truncate? and :signal, as send! takes them, but with
    url/public? as the :allow-pred by default
  - :send, the function that sends each request, send! by default"
  ([url]
   (probe! url {}))
  ([url {:keys [headers send] :or {send send!} :as opts}]
   (let [answer!       (fn [request]
                         (-> (try-send! send (assoc request :url url)
                                        (public-by-default opts))
                             (async/then sent-facts)))
         head-request  {:method  :head
                        :headers (merge {"Accept-Encoding" "identity"} headers)}
         range-request {:range [0 1] :as :bytes :headers headers}]
     (async/then (answer! head-request)
                 (fn [head]
                   (async/then (answer! range-request)
                               (fn [got] {:head head :get got})))))))

(comment
  (authorization {:username "ann" :password "secret"})
  (send! {:url "https://example.org/episode.mp3" :range [0 1023] :as :bytes})
  (probe! "https://example.org/episode.mp3")
  #_.)

#?(:clj
   (comment
     (select-keys (fetch! "https://example.org/feed.xml")
                  [:status :url :etag :last-modified])
     #_.))
