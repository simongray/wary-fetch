(ns dk.simongray.wary-fetch.proxy-test
  "The proxy for the requests of a web page: what goes on to the other
  site, what comes back to the page, what it refuses, its failures as the
  exceptions of http/send!, and fetch! through it, with the HTTP between
  the page and the proxy played in memory."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.async :as async]
            [dk.simongray.wary-fetch.limits :as limits]
            [dk.simongray.wary-fetch.proxy :as proxy]
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.wary-fetch.test-support :refer [run utf8 #?(:clj with-file-server)]]
            [dk.simongray.wary-fetch.url :as url]
            [dk.simongray.webcrypto.bytes :as bytes]))

(def feed-xml
  "<rss version=\"2.0\"><channel><title>Blåbær</title><item><title>E1</title><guid>1</guid></item></channel></rss>")

(def mp3
  "The first bytes of a media file."
  (bytes/of [0x49 0x44 0x33 0x04]))

(def answers
  "What the other sites answer, by URL, as http/send! gives it."
  {"https://a.example/feed.xml" {:status        200
                                 :url           "https://b.example/feed.xml"
                                 :permanent-url "https://b.example/feed.xml"
                                 :headers       {"content-type" ["application/rss+xml; charset=ISO-8859-1"]
                                                 "etag"         ["\"v1\""]
                                                 "set-cookie"   ["session=theirs"]}
                                 :body          feed-xml}
   "https://a.example/ep.mp3"   {:status  206
                                 :url     "https://a.example/ep.mp3"
                                 :headers {"content-type" ["audio/mpeg"] "content-range" ["bytes 0-3/1000"]}
                                 :body    mp3}
   "https://a.example/plain"    {:status  301
                                 :url     "https://a.example/plain"
                                 :headers {"location" ["http://a.example/plain"]}
                                 :body    ""}})

(defn- upstream
  "A stand-in for http/send! that answers from answers, notes each request
  in the atom `sent`, and throws as send! does for a URL it doesn't know."
  [sent]
  (fn [{:keys [url] :as request}]
    (swap! sent conj request)
    (or (answers url)
        (case url
          "https://slow.example/" (throw (ex-info "Timed out" {:type ::http/timeout :url url}))
          "https://big.example/"  (throw (ex-info "Too large" {:type ::http/too-large :url url}))
          (throw (limits/refusal url))))))

(defn- server
  "The proxy with `send` as its :send: handler on the JVM, and in
  ClojureScript the same parts as handler joins them."
  [send]
  #?(:clj  (proxy/handler {:send send})
     :cljs (let [opts (merge proxy/defaults {:send send})]
             (fn [ring-request]
               (or (proxy/refusal ring-request)
                   (try
                     (proxy/ring-response (send (proxy/upstream-request ring-request opts)) opts)
                     (catch :default e
                       (proxy/error-response e))))))))

(defn- over-http
  "A send that takes each request to the Ring handler `handler` as a
  browser and a Ring adapter would: the headers in lower case, with the
  Range of :range, and the body as UTF-8 text unless the request is for
  bytes."
  [handler]
  (fn [{:keys [method url headers range as]}]
    (let [lower    (fn [m f] (into {} (map (fn [[k v]] [(str/lower-case k) (f v)])) m))
          {:keys [status headers body]}
          (handler {:request-method (or method :get)
                    :query-string   (second (re-find #"\?(.*)$" url))
                    :headers        (cond-> (lower headers secret/reveal)
                                      range (assoc "range" (str "bytes=" (first range) "-" (second range))))})]
      {:status  status
       :url     url
       :headers (lower headers #(if (string? %) [%] (vec %)))
       :body    (when (and body (pos? (bytes/length body)))
                  (if (= :bytes as) body (utf8 body)))})))

(deftest the-request-that-goes-on
  (let [ring    {:request-method :get
                 :query-string   "url=https%3A%2F%2Fa.example%2Fep.mp3&as=bytes"
                 :headers        {"accept"        "*/*"
                                  "if-none-match" "\"v1\""
                                  "range"         "bytes=0-3"
                                  "cookie"        "session=ours"
                                  "authorization" "Bearer ours"
                                  "user-agent"    "A browser"}}
        request (proxy/upstream-request ring)]
    (is (= ["https://a.example/ep.mp3" :get :bytes [0 3]] ((juxt :url :method :as :range) request)))
    (is (= [{"accept" "*/*" "if-none-match" "\"v1\"" "Via" "1.1 wary-fetch"} http/user-agent]
           ((juxt :headers :user-agent) request))
        "neither the page's cookies, its credentials nor its User-Agent go on")
    (is (= [url/public? (* 32 1024 1024)]
           ((juxt :allow-pred :max-bytes) request)))
    (is (nil? (:as (proxy/upstream-request (assoc ring :query-string "url=https%3A%2F%2Fa.example%2F"))))
        "text, unless the page asks for bytes")
    (is (nil? (proxy/upstream-request (assoc ring :query-string "url=file%3A%2F%2F%2Fetc%2Fpasswd"))))
    (is (= [{"accept" "*/*" "Via" "1.1 wary-fetch"} "Mine"]
           ((juxt :headers :user-agent)
            (proxy/upstream-request ring {:forward #{"accept"} :user-agent "Mine"})))
        "an option replaces its default, and the others stay")))

(deftest the-answer-that-comes-back
  (let [{:keys [status headers body]} (proxy/ring-response (answers "https://a.example/feed.xml"))]
    (is (= 200 status))
    (is (= "application/rss+xml;charset=utf-8" (get headers "Content-Type")) "the text goes as UTF-8")
    (is (= feed-xml (utf8 body)))
    (is (= ["\"v1\"" nil] [(get headers "etag") (get headers "set-cookie")]) "only the headers that it exposes")
    (is (= ["https://b.example/feed.xml" "https://b.example/feed.xml"]
           [(get headers "Content-Location") (get headers proxy/permanent-header)]))
    (is (= "default-src 'none'; sandbox" (get headers "Content-Security-Policy")) "a page that opens it runs no scripts"))
  (is (= mp3 (:body (proxy/ring-response (answers "https://a.example/ep.mp3")))) "bytes go as they came")
  (is (= ["image/png" "application/feed+json;charset=utf-8" "text/vtt;charset=utf-8;x=y"]
         (for [type ["image/png" "application/feed+json" "text/vtt; charset=ISO-8859-1; x=y"]]
           (get-in (proxy/ring-response {:status 200 :headers {"content-type" [type]} :body "…"})
                   [:headers "Content-Type"])))
      "a charset only on a type of text, so a page that didn't ask for bytes isn't told that an image is text")
  (let [{:keys [status headers]} (proxy/ring-response {:status  302
                                                       :url     "https://a.example/old/ep.mp3"
                                                       :headers {"location" ["/ep.mp3"]}})]
    (is (= 302 status) "a redirect that the client didn't follow")
    (is (= [nil "https://a.example/ep.mp3"] [(get headers "Location") (get headers proxy/location-header)])
        "with its Location under a name that the browser doesn't follow, made absolute"))
  (is (nil? (get (:headers (proxy/ring-response (answers "https://a.example/feed.xml") {:expose #{"content-type"}})) "etag"))
      "only the headers that the options expose"))

(deftest what-the-proxy-refuses
  (let [ring   {:request-method :get :query-string "url=https%3A%2F%2Fa.example%2Ffeed.xml" :headers {"sec-fetch-mode" "cors" "sec-fetch-site" "same-origin"}}
        status #(some-> (proxy/refusal %) :status)]
    (is (nil? (status ring)) "a request of a page of its own site")
    (is (nil? (status (assoc ring :request-method :head))))
    (is (= 405 (status (assoc ring :request-method :post))))
    (is (= 502 (status (assoc-in ring [:headers "via"] "1.1 cdn, 1.1 wary-fetch"))) "a request that the proxy sent itself")
    (is (= 403 (status (assoc-in ring [:headers "sec-fetch-mode"] "navigate"))) "a page that opens it")
    (is (= 403 (status (assoc ring :headers {"Sec-Fetch-Mode" "navigate"}))) "by any case of the header's name")
    (is (= 403 (status (assoc-in ring [:headers "sec-fetch-site"] "cross-site"))) "a page of another site")
    (is (= 400 (status (assoc ring :query-string "url=javascript%3Aalert(1)"))))
    (is (str/starts-with? (get-in (proxy/refusal (assoc ring :request-method :put)) [:headers "Proxy-Status"]) "wary-fetch; error="))))

(deftest failures-come-back-as-the-exceptions-of-call
  (let [sent (atom [])
        send (proxy/sender "https://app.example/proxy" {:send (over-http (server (upstream sent)))})
        kind (fn [url]
               (async/then (async/attempt #(send {:url url}))
                           (fn [{:keys [error]}] [(:type (ex-data error)) (http/timeout? error) (ex-message error)])))]
    (run #(async/then (async/all [(kind "https://private.example/") (kind "https://slow.example/") (kind "https://big.example/")])
                      (fn [[refused slow big]]
                        (is (= [::http/refused false "The request to https://private.example/ is not allowed"] refused))
                        (is (= [::http/timeout true "Timed out"] slow))
                        (is (= ::http/too-large (first big))))))))

(deftest fetching-a-document-through-the-proxy
  (let [sent (atom [])
        send (proxy/sender "https://app.example/proxy" {:send (over-http (server (upstream sent)))})]
    (run #(async/then (http/fetch! "https://a.example/feed.xml" {:send send :credentials {:username "ann" :password "p"}})
                      (fn [{:keys [status url permanent-url etag body]}]
                        (is (= [200 "https://b.example/feed.xml" "https://b.example/feed.xml" "\"v1\""] [status url permanent-url etag])
                            "the answer as http/send! gives it, with the URLs that the redirects led to")
                        (is (str/includes? body "<title>Blåbær</title>"))
                        (is (not (contains? (:headers (first @sent)) "authorization"))
                            "the credentials of a document don't go on, unless :forward names them")))
         #(async/then (async/attempt (fn [] (http/fetch! "https://private.example/feed.xml" {:send send})))
                      (fn [{:keys [error]}]
                        (is (= ::http/refused (:type (ex-data error))))
                        (is (str/includes? (ex-message error) "is not allowed") "a refusal of the proxy, as send! reports one")))
         #(do (reset! sent [])
              (async/then (send {:url "https://a.example/ep.mp3" :as :bytes :range [0 3]})
                          (fn [{:keys [status body]}]
                            (is (= [206 mp3] [status body]))
                            (is (= [:bytes [0 3]] ((juxt :as :range) (first @sent))) "the bytes and the range that the page asked for"))))
         #(async/then (send {:url "https://a.example/plain"})
                      (fn [{:keys [status headers]}]
                        (is (= [301 ["http://a.example/plain"]] [status (get headers "location")])
                            "a redirect that wasn't followed, from https to http, as a direct request gives it"))))))

(deftest requests-that-are-not-http
  (let [sent (atom [])
        send (proxy/sender "/proxy" {:send #(do (swap! sent conj %) {:messages []})})]
    (send {:url "wss://socket.example/" :message "hello"})
    (is (= [{:url "wss://socket.example/" :message "hello"}] @sent) "go to the :send as they are")))

#?(:clj
   (defn- streamed
     "The answer of the streaming proxy with `opts` to a GET of `url` with
     the Ring `headers`, with its body read whole into bytes."
     [opts url headers]
     (let [handler  (proxy/stream-handler opts)
           response (handler {:request-method :get
                              :query-string   (str "url=" (url/percent-encode url))
                              :headers        headers})]
       (update response :body #(if (instance? java.io.InputStream %)
                                 (with-open [in ^java.io.InputStream %] (.readAllBytes in))
                                 %)))))

#?(:clj
   (deftest streaming-through-the-proxy
     (let [episode (bytes/of (map #(mod % 256) (range 300000)))]
       (with-file-server episode {}
         (fn [url]
           (let [local  {:allow-pred (constantly true)}
                 whole  (streamed local url {})
                 part   (streamed local url {"range" "bytes=10-19"})
                 capped (streamed (assoc local :max-bytes 1000) url {})]
             (is (= [200 "300000"] [(:status whole) (get-in whole [:headers "Content-Length"])]))
             (is (bytes/same? episode (:body whole)) "the body as it came, passed on")
             (is (= "default-src 'none'; sandbox" (get-in whole [:headers "Content-Security-Policy"])))
             (is (= [206 "bytes 10-19/300000" (range 10 20)]
                    [(:status part) (get-in part [:headers "content-range"]) (vec (:body part))])
                 "a range for a download to resume, which goes on as it is")
             (is (= [502 true] [(:status capped)
                                (str/includes? (get-in capped [:headers "Proxy-Status"]) "http_response_body_size")])
                 "an answer over the limit, refused before its body")
             (is (= 403 (:status (streamed {} url {}))) "a host on the local network, which url/public? refuses")))))))
