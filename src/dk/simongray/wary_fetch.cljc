(ns dk.simongray.wary-fetch
  "Requests as maps, and the clients that send them.

  On the JVM, the client is java.net.http, and in Node it's node:http.
  Either follows redirects, accepts gzip, makes conditional requests, and
  decodes the body by the charset that the server or the document
  declares. In a browser, the client is fetch.

  A request is a map of :method, :url, :headers and :body, and it's a GET
  unless :method names another method. Any HTTP client can send a request
  of this shape. A response is a plain map, whose :url is the final URL
  after redirects.

  A request moves no more bytes than its purpose needs."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.async :as async]
            #?(:cljs [dk.simongray.wary-fetch.browser :as browser])
            [dk.simongray.wary-fetch.bytes :as bytes]
            [dk.simongray.wary-fetch.headers :as headers]
            #?(:clj [dk.simongray.wary-fetch.jvm :as jvm])
            [dk.simongray.wary-fetch.limits :as limits]
            #?(:cljs [dk.simongray.wary-fetch.node :as node])
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.wary-fetch.url :as url])
  #?(:clj (:import [java.net.http HttpTimeoutException])))

(def user-agent
  "The User-Agent of a request outside a browser, unless the request names
  another."
  "wary-fetch (+https://github.com/simongray/wary-fetch)")

(defn- base64
  "The UTF-8 bytes of `s` in base64."
  [s]
  (bytes/base64 (bytes/encode s)))

;; RFC 7617, section 2 (Basic); RFC 6750, section 2.1 (Bearer)
(defn authorization
  "The Authorization header for `credentials`, or nil for no credentials.

  The `credentials` are a map, or a function of no arguments that gives
  one. The map has :username and :password for basic authentication, or
  :token for a bearer token. The password and the token can be hidden
  texts, so that a map of credentials doesn't show them when it's
  printed.

  The header has the password or the token, so it's a hidden text too,
  and a request map with it doesn't show it when printed."
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
  "The `request` with the text of each hidden header and of a hidden body,
  as the server needs them.

  The headers Authorization and Cookie are hidden, and so is the body of a
  request with a secret. Another client needs their texts, and a request
  that's revealed shows its credentials when it's printed."
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
  "The `headers` of a request for the bytes `range`, with the Range header,
  and with the User-Agent `agent` unless they name another or the browser
  sends its own.

  On the JVM and in Node, a request for a range asks for bytes that aren't
  gzipped, so that the offsets are those of the file."
  [headers range agent]
  (cond-> (merge #?(:clj  {"User-Agent" agent}
                    :cljs (when-not browser/available? {"User-Agent" agent}))
                 headers)
    range (assoc "Range" (str "bytes=" (first range) "-" (second range)))
    #?@(:clj  [range (assoc "Accept-Encoding" "identity")]
        :cljs [(and range node?) (assoc "Accept-Encoding" "identity")])))

(defn send!
  "Send the `request`, and give the response as a map of :status, :url,
  :headers and :body. On the JVM it gives the response, and in
  ClojureScript a promise of it.

  The request is a map of :method, :url, :headers and :body. It's a GET
  unless :method names another method, and it goes with the :body, a
  string or bytes, when it has one. A header and a body can be hidden
  texts, and they go out as their texts. The body of the response is
  text, with gzip undone. It's absent for a HEAD request and for an empty
  body.

  The `request` can also have these keys:

  - :as :bytes, for a body of bytes, e.g. of a media file
  - :timeout-seconds, the limit of the whole exchange, 30 by default
  - :max-bytes, the limit of the body, 100 MB by default, or nil for no
    limit
  - :range, a pair of the first and the last byte to ask for, as RFC
    9110, section 14.1.2, has it. A nil last byte asks for the rest.
  - :allow-pred, a predicate of a URL, as url/public? is
  - :user-agent, the User-Agent unless the :headers name one, user-agent
    by default

  An exchange that goes beyond a limit throws ::timeout or ::too-large.
  On the JVM, the client of the JDK throws its HttpTimeoutException while
  it waits for the headers. When :allow-pred refuses the URL of the
  request, it throws ::refused.

  A server that sends the range answers 206, and the response then has
  the :content-range, a map of :start, :end and :total. A server that
  ignores ranges answers 200 with the whole file. The body is read no
  further than the end of the range, when it has one. From another
  answer, e.g. the page of a 404, no more bytes are read than the range
  names.

  On the JVM and in Node, redirects are followed, five at most, and never
  from https to http. A redirect to another origin leaves the credentials
  behind, and :allow-pred must take the URL of each redirect. The
  :redirects of the response list each redirect as a vector of its
  status, the URL that answered with it, and the headers of its answer.
  When the redirects begin with permanent ones, :permanent-url is where
  the URL moved for good. On the JVM, the URL is made legal first.

  In Node, the name of a host is looked up before :allow-pred is asked,
  and the request connects only to the addresses that it found, so that
  url/public? checks the addresses that the request goes to. Each request
  has a connection of its own, since one of a shared pool can lead to an
  address that nobody checked for it.

  In a browser, and in a runtime without the modules of Node, fetch sends
  the request. It follows redirects itself and doesn't report them, so
  :allow-pred doesn't see them. A browser lets a page read an answer from
  another site only as far as the CORS headers of the answer allow. It
  hides the headers that the answer doesn't expose, e.g. Content-Range
  and ETag, and :filtered? marks such an answer. A browser also sends its
  own User-Agent, so :user-agent counts only outside one."
  [{:keys [method headers timeout-seconds allow-pred]
    :or   {timeout-seconds 30 allow-pred (constantly true)}
    :as   request}]
  (let [method  (some-> method name str/lower-case keyword)
        agent   (or (:user-agent request) user-agent)
        request (-> (merge {:max-bytes limits/default-max-bytes} request)
                    (assoc :method  method
                           :headers (request-headers headers (:range request)
                                                     agent))
                    revealed)]
    #?(:clj  (jvm/send! request timeout-seconds allow-pred)
       :cljs (if node?
               (node/send! request timeout-seconds allow-pred)
               (browser/send! request timeout-seconds allow-pred)))))

(defn send-public!
  "Send the `request`, but only to a host on the public internet. On the
  JVM and in Node, the host of each redirect must be public too.

  It sends no request to the network of the app or of its device, and
  throws ::refused instead, or in ClojureScript rejects with it."
  [request]
  (send! (assoc request :allow-pred url/public?)))

(def ^:no-doc option-keys
  "The keys of a request that say how it's sent, which the functions that
  send take from their options."
  [:timeout-seconds :max-bytes :allow-pred :user-agent])

(defn with-options
  "The `request` with the :timeout-seconds, :max-bytes, :allow-pred and
  :user-agent that `opts` give, for a function that sends with the options
  of send!."
  [request opts]
  (into request (remove (comp nil? val)) (select-keys opts option-keys)))

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
  the :type, :url and :status that it holds."
  [e]
  (into {:reason (async/explain e)}
        (select-keys (ex-data e) [:type :url :status])))

(defn try-send!
  "Send the `request` with `send`, a function such as send!, and give a
  map of the response as the :value, or of the exception as the :error
  where `send` throws. The `opts` are the :timeout-seconds, :max-bytes,
  :allow-pred and :user-agent to put in the request.

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
  give none, and the request goes to the URL without them."
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
      opts))))

(defn- now
  "The instant now, by the clock of the platform."
  []
  #?(:clj  (java.util.Date.)
     :cljs (js/Date.)))

(defn fetch!
  "GET the document at `url` with `opts`, and give the response with the
  body as text, in ClojureScript as a promise.

  The response also has :etag and :last-modified when the server gives
  them. It has :not-before when the server says how long to wait.

  The `opts` are these:

  - :etag and :last-modified from an earlier response. They go as
    If-None-Match and If-Modified-Since, and a 304 then comes back with
    no body.
  - :credentials, as authorization takes them
  - :accept, the Accept header, */* by default
  - :timeout-seconds, :max-bytes, :allow-pred and :user-agent, as send!
    takes them
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
     "GET `url` with `opts`, and give the digests of its body by the
     MessageDigest `algorithms`, e.g. SHA-384.

     The result has the :status and the :url. For a 200, it also has the
     :digests, which map each algorithm to the bytes of its digest. The
     body streams through the digests, and none of it is kept, so it can
     be a large media file.

     The `opts` are these:

     - :headers of the request
     - :allow-pred and :user-agent, as send! takes them
     - :timeout-seconds, the limit of the whole exchange, 600 by default"
     ([url algorithms]
      (digest! url algorithms {}))
     ([url algorithms {:keys [headers timeout-seconds allow-pred]
                       :or   {timeout-seconds 600 allow-pred (constantly true)}
                       :as   opts}]
      (let [agent   (or (:user-agent opts) user-agent)
            request {:url     url
                     :method  :get
                     :headers (merge {"User-Agent"      agent
                                      "Accept-Encoding" "identity"}
                                     headers)}]
        (jvm/digests! (revealed request) algorithms timeout-seconds
                      allow-pred)))))

(defn- whole-size
  "The size of the file whose first `n` bytes came in a 200 to a request
  for the first `asked`, by the `content-length` and `content-encoding`
  of the answer.

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

;; RFC 9110, section 14.4
(defn- unsatisfied-total
  "The size of the whole file that the Content-Range value `s` of an
  answer of 416 gives, or nil when it gives none."
  [s]
  (some->> s (re-matches #"(?i)\s*bytes\s+\*/(\d{1,15})\s*") second parse-long))

(defn- failed
  "The exception for the answer to a request for the file at `url` whose
  `status` is a failure."
  [url status]
  (ex-info (str "The file answered " status ": " url)
           {:type ::failed :status status :url url}))

(defn fetcher
  "A function that fetches the bytes of the file at `url` by range, with
  `opts`.

  The function takes a start and an end, with the end excluded. It gives
  a map of :bytes, :start and :total, or in ClojureScript a promise of
  it. You can wrap it to log or keep what it fetches. It throws as a
  request does, and it throws ::failed with the :status for an answer
  that's a failure.

  A server that ignores ranges sends the file from its start. Then only
  the bytes up to the end are read, and the answer is :whole?. A server
  answers 416 for a range past the end of the file. Then the function
  gives no bytes, and the size of the file, so that a reader can tell
  that the file is shorter than it says. In a browser, the total is known
  only when the server exposes Content-Range.

  The `opts` are these:

  - :headers of the requests
  - :timeout-seconds, :max-bytes, :allow-pred and :user-agent, as send!
    takes them
  - :send, a function that sends a request map in place of send!, as
    send! does"
  ([url]
   (fetcher url {}))
  ([url {:keys [headers send] :or {send send!} :as opts}]
   (fn [start end]
     (let [request {:url     url
                    :headers headers
                    :as      :bytes
                    :range   [start (dec end)]}]
       (async/then
        (send (with-options request opts))
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
                   :total (unsatisfied-total (h "content-range"))}
              (throw (failed url status))))))))))

(defn facts
  "What the `response` tells about its file, as far as it tells it.

  The facts have these keys:

  - :status, :url and :permanent-url, as in the response
  - :redirects, each a map of :status, :url and :cors, with the URL that
    answered with it
  - :location, the Location of a redirect that wasn't followed
  - :type, the media type, in lower case and without its parameters
  - :length, what Content-Length gives
  - :range, the Content-Range, as a map of :start, :end and :total
  - :encoding, :etag and :last-modified, from their headers
  - :cors, the origins that Access-Control-Allow-Origin allows to read
    the answer, with all its values joined, as a browser joins them
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
           :type          (headers/media-type (h "content-type"))
           :length        length
           :range         (:content-range response)
           :encoding      (h "content-encoding")
           :etag          (h "etag")
           :last-modified (h "last-modified")
           :cors          (cors (:headers response))
           :filtered?     (:filtered? response)})))

(defn- sent-facts
  "The facts of the response of an `attempt` to send, or a map with its
  failure under :error for a request that failed."
  [{:keys [value error] :as attempt}]
  (if error
    {:error (failure error)}
    (facts value)))

(defn probe!
  "Ask the server about the file at `url` with `opts`, and give the facts,
  while no more than two bytes of the file move. On the JVM it gives
  them, and in ClojureScript a promise of them.

  The result is a map of :head and :get, the facts of the answers to a
  HEAD request and to a request for the first two bytes. The facts of a
  request that failed are its exception under :error, as failure gives
  it.

  Most servers answer the second request with 206, and with the size of
  the whole file in Content-Range. A server that ignores ranges answers
  200, and two bytes of that answer are read. The HEAD request asks for
  the file as it is, not gzipped, so that its Content-Length is the size
  of the file. The two requests go one after the other, so a server sees
  one at a time for one file.

  The `opts` are these:

  - :headers of the two requests, e.g. an Origin to learn about CORS
  - :timeout-seconds, :max-bytes, :allow-pred and :user-agent, as send!
    takes them
  - :send, a function that sends a request map in place of send!, as
    send! does"
  ([url]
   (probe! url {}))
  ([url {:keys [headers send] :or {send send!} :as opts}]
   (let [answer!       (fn [request]
                         (-> (try-send! send (assoc request :url url) opts)
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
