(ns dk.simongray.wary-fetch-test
  "Sending: requests and their credentials, fetch!, the fetcher, probe!
  and the facts of an answer, on both platforms, and on the JVM against
  servers on localhost."
  (:require [clojure.test :refer [deftest is testing #?(:cljs async)]]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.async :as async]
            #?(:cljs [dk.simongray.wary-fetch.browser :as browser])
            [dk.simongray.wary-fetch.redirect :as redirect]
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.wary-fetch.test-support :refer [run #?(:clj with-file-server)]]
            [dk.simongray.wary-fetch.url :as url])
  #?(:clj (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
                   [java.io ByteArrayOutputStream]
                   [java.net InetSocketAddress]
                   [java.security MessageDigest]
                   [java.util.zip GZIPOutputStream])))

(def local
  "The options of a request to a test server on this machine, which the
  functions that fetch refuse by default."
  {:allow-pred (constantly true)})

(deftest facts-of-an-answer
  (is (= {:status 206 :url "https://b/e.mp3" :type "audio/mpeg" :length 2 :range {:start 0 :end 1 :total 1000}
          :etag "\"1\"" :cors "*" :permanent-url "https://b/e.mp3"
          :redirects [{:status 301 :url "https://a/e.mp3" :cors "*"}]}
         (http/facts {:status        206
                      :url           "https://b/e.mp3"
                      :redirects     [[301 "https://a/e.mp3" {"access-control-allow-origin" ["*"]}]]
                      :permanent-url "https://b/e.mp3"
                      :content-range {:start 0 :end 1 :total 1000}
                      :headers       {"content-type"                ["Audio/MPEG; charset=binary"]
                                      "content-length"              ["2"]
                                      "etag"                        ["\"1\""]
                                      "access-control-allow-origin" ["*"]}})))
  (is (= {:status 404} (http/facts {:status 404 :headers {}})) "only what the answer tells")
  (is (= "*, *" (:cors (http/facts {:status 200 :headers {"access-control-allow-origin" ["*" "*"]}})))
      "two values, which a browser reads as one that allows no page")
  (is (= [200 "text/vtt" 12] ((juxt :status :type :length)
                              (http/facts {:status 200 :headers {"content-type" "text/vtt" "content-length" "12"}})))
      "headers as strings, as an app's own client can give them")
  (is (nil? (:length (http/facts {:status 200 :headers {"content-length" ["-1"]}})))))

#?(:cljs
   (deftest fetching-by-range-with-fetch
     (async done
       (let [real        (.-fetch js/globalThis)
             node?       http/node?
             pulls       (atom 0)
             whole       (js/ReadableStream. #js {:pull (fn [controller]
                                                          (if (< (swap! pulls inc) 100)
                                                            (.enqueue controller (js/Uint8Array. 65536))
                                                            (.close controller)))})
             answers     (atom [(js/Response. (js/Uint8Array. 10)
                                              #js {:status 206 :headers #js {"content-range" "bytes 5-14/1000"}})
                                (js/Response. whole
                                              #js {:status 200 :headers #js {"content-length" (str (* 99 65536))}})])
             fetch-bytes (http/fetcher "https://example.org/a.mp3" {})]
         ;; the global fetch, which sends outside Node
         (set! http/node? false)
         (set! (.-fetch js/globalThis) (fn [_ _]
                                         (let [res (first @answers)]
                                           (swap! answers rest)
                                           (js/Promise.resolve res))))
         (-> (fetch-bytes 5 15)
             (.then (fn [{b :bytes :keys [start total]}]
                      (is (= [10 5 1000] [(.-length b) start total]) "a range")
                      (fetch-bytes 0 100000)))
             (.then (fn [{b :bytes :keys [start total]}]
                      (is (= [100000 0 (* 99 65536)] [(.-length b) start total]) "the start of the whole file")
                      (is (< @pulls 10) "the rest of the file left unread")))
             (.catch (fn [e] (is false (str "rejected: " e))))
             (.finally (fn []
                         (set! (.-fetch js/globalThis) real)
                         (set! http/node? node?)
                         (done))))))))

#?(:cljs
   (deftest probing-with-fetch
     (async done
       (let [real     (.-fetch js/globalThis)
             node?    http/node?
             requests (atom [])
             agents   (atom #{})]
         ;; the global fetch, which sends outside Node
         (set! http/node? false)
         (set! (.-fetch js/globalThis)
               (fn [_ init]
                 (swap! requests conj [(.-method init) (or (aget (.-headers init) "Range") nil)])
                 (swap! agents conj (aget (.-headers init) "User-Agent"))
                 (js/Promise.resolve
                  (if (= "HEAD" (.-method init))
                    (js/Response. nil #js {:status 200 :headers #js {"content-type" "audio/mpeg" "content-length" "1000"}})
                    (js/Response. (js/Uint8Array. 2) #js {:status 206 :headers #js {"content-type" "audio/mpeg"
                                                                                     "content-range" "bytes 0-1/1000"}})))))
         (-> (http/probe! "https://a.example/e.mp3")
             (.then (fn [{:keys [head get]}]
                      (is (= [200 "audio/mpeg" 1000] ((juxt :status :type :length) head)))
                      (is (= [206 {:start 0 :end 1 :total 1000}] ((juxt :status :range) get)))
                      (is (= #{["HEAD" nil] ["GET" "bytes=0-1"]} (set @requests)))
                      (is (= (if browser/available? #{nil} #{http/user-agent}) @agents)
                          "each request names the library, unless a browser sends its own User-Agent")))
             (.catch (fn [e] (is false (str "rejected: " e))))
             (.finally (fn []
                         (set! (.-fetch js/globalThis) real)
                         (set! http/node? node?)
                         (done))))))))

(deftest credentials-as-a-header
  (let [header (comp secret/reveal http/authorization)]
    (is (= "Basic dTpw" (header {:username "u" :password "p"})))
    (is (= "Bearer t" (header (fn [] {:token "t"}))) "a function is asked for the current ones")
    (is (nil? (http/authorization nil)))
    (testing "a password and a token that are hidden"
      (is (= "Basic dTpw" (header {:username "u" :password (secret/hide "p")})))
      (is (= "Bearer t" (header {:token (secret/hide "t")})))
      (is (= "Bearer t" (header (fn [] {:token (secret/hide "t")}))))
      (is (not (re-find #"hunter2" (pr-str {:username "u" :password (secret/hide "hunter2")})))))
    (testing "the header is a hidden text"
      (is (secret/hidden? (http/authorization {:username "u" :password "p"})))
      (is (= "#<secret>" (pr-str (http/authorization {:token "t"})))))))

(deftest a-request-for-another-client
  (let [request {:method  :post
                 :url     "https://example.org/x"
                 :headers {"Authorization" (http/authorization {:token "hunter2"})
                           "Cookie"        (secret/hide "sessionid=abc")
                           "Accept"        "*/*"}
                 :body    (secret/hide "hub.secret=s3cr3t")}]
    (is (not (re-find #"hunter2|sessionid|s3cr3t" (pr-str request))))
    (is (= {:method  :post
            :url     "https://example.org/x"
            :headers {"Authorization" "Bearer hunter2" "Cookie" "sessionid=abc" "Accept" "*/*"}
            :body    "hub.secret=s3cr3t"}
           (http/revealed request)))
    (is (= (http/revealed request) (http/revealed (http/revealed request))))
    (is (= {:url "https://example.org/x"} (http/revealed {:url "https://example.org/x"})) "a request without headers and a body")
    (testing "a redirect to another origin leaves the hidden headers behind too"
      (is (= {"Accept" "*/*"} (:headers (redirect/next-request request 307 "https://other.example/x"))))
      (is (= (:headers request) (:headers (redirect/next-request request 307 "https://example.org/y")))))))

(deftest credentials-in-a-url
  (is (= ["https://host.example:8443/feed.xml?x=1" {:username "simon@example.org" :password (secret/hide "p p")}]
         (url/split-credentials "https://simon%40example.org:p%20p@host.example:8443/feed.xml?x=1"))
      "decoded, and the URL without them")
  (let [[url credentials] (url/split-credentials "https://simon:hunter2@host.example/feed.xml")]
    (is (= "https://host.example/feed.xml" url))
    (is (= "hunter2" (secret/reveal (:password credentials))))
    (is (= "{:username \"simon\", :password #<secret>}" (pr-str credentials)) "the password does not show in print")
    (is (= "Basic c2ltb246aHVudGVyMg==" (secret/reveal (http/authorization credentials)))))
  (is (= ["https://host.example/" {:username "user" :password (secret/hide "")}] (url/split-credentials "https://user@host.example/")))
  (is (= ["https://host.example/a@b" nil] (url/split-credentials "https://host.example/a@b")) "an @ in the path is no user")
  (is (= ["https://host.example/feed.xml" nil] (url/split-credentials "https://host.example/feed.xml"))))

(deftest the-request-of-a-fetch
  (let [request (http/fetch-request "https://u:p@example.org/feed.xml"
                                    {:etag "\"v1\"" :last-modified "Tue, 29 Sep 2026 10:00:00 GMT" :max-bytes 100 :max-pages 3})]
    (is (= "https://example.org/feed.xml" (:url request)) "the URL without the credentials")
    (is (= {"If-None-Match" "\"v1\"" "If-Modified-Since" "Tue, 29 Sep 2026 10:00:00 GMT" "Authorization" "Basic dTpw"}
           (select-keys (:headers (http/revealed request)) ["If-None-Match" "If-Modified-Since" "Authorization"])))
    (is (= [100 false] [(:max-bytes request) (contains? request :max-pages)]) "only the options of send!")
    (is (= {:url "https://example.org/feed.xml" :headers {"Accept" "*/*"} :allow-pred url/public?}
           (http/fetch-request "https://example.org/feed.xml"))
        "and without options, the plain request, to the public internet only")
    (is (= local (select-keys (http/fetch-request "https://example.org/feed.xml" local) [:allow-pred]))
        "unless the options allow more")
    (is (= "Mine" (:user-agent (http/fetch-request "https://example.org/feed.xml" {:user-agent "Mine"})))
        "the User-Agent, which send! puts in the headers")
    (is (= "application/rss+xml" (get-in (http/fetch-request "https://example.org/feed.xml" {:accept "application/rss+xml"})
                                         [:headers "Accept"])))))

(deftest fetching-through-a-send
  (let [sent (atom nil)
        send (fn [request]
               (reset! sent request)
               {:status 304 :url (:url request) :headers {"etag" ["\"v1\""] "retry-after" ["120"]}})]
    (run #(async/then (http/fetch! "https://example.org/feed.xml" {:etag "\"v1\"" :send send})
                      (fn [response]
                        (is (= "\"v1\"" (get-in @sent [:headers "If-None-Match"])) "the request of fetch-request")
                        (is (= [304 "\"v1\""] ((juxt :status :etag) response)))
                        (is (inst? (:not-before response))))))))

#?(:clj
   (defn- gzip
     ^bytes [^bytes body]
     (let [out (ByteArrayOutputStream.)]
       (with-open [z (GZIPOutputStream. out)]
         (.write z body))
       (.toByteArray out))))

#?(:clj
   (defn- with-server
     "Call `f` with the base URL of a server on localhost answering `routes`, a
     map of path to `[status headers body-bytes]`."
     [routes f]
     (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
       (doseq [[path [status headers ^bytes body]] routes]
         (.createContext server path
                         (reify HttpHandler
                           (handle [_ exchange]
                             (doseq [[k v] headers]
                               (.add (.getResponseHeaders exchange) k v))
                             (.sendResponseHeaders exchange status (alength body))
                             (with-open [out (.getResponseBody exchange)]
                               (.write out body))))))
       (.start server)
       (try
         (f (str "http://127.0.0.1:" (.getPort (.getAddress server))))
         (finally
           (.stop server 0))))))

#?(:clj
   (deftest fetching-from-a-server
     (let [xml   "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rss version=\"2.0\"><channel><title>Blåbær</title></channel></rss>"
           bytes (.getBytes xml "UTF-8")
           big   (byte-array (* 2 1024 1024))]
       (with-server {"/feed.xml" [200 {"Content-Type" "application/rss+xml; charset=UTF-8" "ETag" "\"v1\""} bytes]
                     "/feed.gz"  [200 {"Content-Type" "application/rss+xml" "Content-Encoding" "gzip"} (gzip bytes)]
                     "/big.xml"  [200 {"Content-Type" "application/rss+xml"} big]
                     "/gone"     [404 {} (.getBytes "nope" "UTF-8")]}
         (fn [base]
           (testing "the body decoded by its charset, with the validator headers picked out"
             (let [{:keys [status body etag]} (http/fetch! (str base "/feed.xml") local)]
               (is (= 200 status))
               (is (= xml body))
               (is (= "\"v1\"" etag))))
           (testing "gzip is undone"
             (is (= xml (:body (http/fetch! (str base "/feed.gz") local)))))
           (testing "a body over :max-bytes is refused rather than read"
             (is (= ::http/too-large
                    (try (http/fetch! (str base "/big.xml") (assoc local :max-bytes 1024)) nil
                         (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
             (is (= (* 2 1024 1024) (count (:body (http/fetch! (str base "/big.xml") local))))))
           (testing "an error status still comes back as a map"
             (is (= 404 (:status (http/fetch! (str base "/gone") local)))))
           (testing "a server on this machine, without an :allow-pred"
             (is (= ::http/refused
                    (try (http/fetch! (str base "/feed.xml")) nil
                         (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
             (is (= ::http/refused (get-in (http/probe! (str base "/feed.xml")) [:head :error :type])))
             (is (= ::http/refused
                    (try ((http/fetcher (str base "/feed.xml")) 0 10) nil
                         (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
             (is (= ::http/refused
                    (try (http/digest! (str base "/feed.xml") ["SHA-256"]) nil
                         (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))))))

#?(:clj
   (defn- with-servers
     "Call `f` with the base URLs of two servers on localhost, two origins,
     that answer each request by `(handler base path exchange)` and note the
     path, the method and the Authorization header of each request in `seen`."
     [seen handler f]
     (let [servers (repeatedly 2 #(HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0))
           bases   (mapv #(str "http://127.0.0.1:" (.getPort (.getAddress ^HttpServer %))) servers)]
       (doseq [[^HttpServer server base] (map vector servers bases)]
         (.createContext server "/"
                         (reify HttpHandler
                           (handle [_ exchange]
                             (let [path (.getPath (.getRequestURI exchange))]
                               (swap! seen conj [base path (.getRequestMethod exchange)
                                                 (.getFirst (.getRequestHeaders exchange) "Authorization")])
                               (handler bases path exchange)))))
         (.start server))
       (try
         (f bases)
         (finally
           (doseq [^HttpServer server servers]
             (.stop server 0)))))))

#?(:clj
   (defn- redirect
     [^HttpExchange exchange status location]
     (.add (.getResponseHeaders exchange) "Location" location)
     ;; with the request body open, Java 11 closes the connection after an
     ;; answer with no body, and the client reuses it
     (.close (.getRequestBody exchange))
     (.sendResponseHeaders exchange status -1)
     (.close exchange)))

#?(:clj
   (deftest the-version-in-the-user-agent
     (let [version #"\(def version\s+\"[^\"]*\"\s+\"([^\"]+)\""]
       (is (= (str "wary-fetch/" (second (re-find version (slurp "build.clj"))))
              http/user-agent)
           "the version that build.clj releases"))))

#?(:clj
   (deftest the-user-agent
     (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
           base   (str "http://127.0.0.1:" (.getPort (.getAddress server)))
           agent  #(:body (http/send! (assoc % :url base)))]
       (.createContext server "/"
                       (reify HttpHandler
                         (handle [_ exchange]
                           (let [headers (.getRequestHeaders exchange)
                                 b       (.getBytes (str (.getFirst headers "User-Agent")) "UTF-8")]
                             (.sendResponseHeaders exchange 200 (alength b))
                             (with-open [out (.getResponseBody exchange)]
                               (.write out b))))))
       (.start server)
       (try
         (is (= http/user-agent (agent {})) "the library's by default")
         (is (= "Mine" (agent {:user-agent "Mine"})))
         (is (= "Theirs" (agent {:user-agent "Mine" :headers {"User-Agent" "Theirs"}}))
             "a header before the option")
         (is (= "Mine" (:body (http/fetch! base (assoc local :user-agent "Mine")))) "an option of fetch! too")
         (finally
           (.stop server 0))))))

#?(:clj
   (deftest following-redirects
     (let [seen (atom [])]
       (with-servers seen
         (fn [[_ other] path ^HttpExchange exchange]
           (case path
             "/away"       (redirect exchange 302 (str other "/feed"))
             "/moved"      (redirect exchange 301 "/moved-again")
             "/moved-again" (redirect exchange 308 "/feed")
             "/for-now"    (redirect exchange 302 "/feed")
             "/then-for-now" (redirect exchange 301 "/for-now")
             "/see-other"  (redirect exchange 303 "/feed")
             "/loop"       (redirect exchange 302 "/loop")
             "/elsewhere"  (redirect exchange 302 "ftp://files.example/feed")
             (do (.sendResponseHeaders exchange 200 2)
                 (with-open [out (.getResponseBody exchange)]
                   (.write out (.getBytes "ok"))))))
         (fn [[base other]]
           (let [call (fn [path & {:as opts}]
                        (reset! seen [])
                        (http/send! (merge {:url (str base path) :headers {"Authorization" "Basic dTpw"}} opts)))]
             (testing "a redirect to another origin leaves the credentials behind"
               (let [r (call "/away")]
                 (is (= [200 (str other "/feed")] [(:status r) (:url r)]))
                 (is (= ["Basic dTpw" nil] (map last @seen)))
                 (is (= [[302 (str base "/away") (str other "/feed")]]
                        (map (fn [[status url headers]] [status url (first (get headers "location"))]) (:redirects r)))
                     "each redirect, with the headers of its answer")))
             (testing "a redirect to a URL of another scheme is not followed"
               (is (= [302 "ftp://files.example/feed"] ((juxt :status #(first (get-in % [:headers "location"])))
                                                        (call "/elsewhere")))))
             (testing "permanent redirects give the URL they lead to"
               (is (= (str base "/feed") (:permanent-url (call "/moved"))))
               (is (= ["Basic dTpw" "Basic dTpw" "Basic dTpw"] (map last @seen)) "the same origin keeps them")
               (is (nil? (:permanent-url (call "/for-now"))) "a temporary one gives none")
               (is (= (str base "/for-now") (:permanent-url (call "/then-for-now")))
                   "and ends the permanent part"))
             (testing "a 303 turns a POST into a GET"
               (call "/see-other" :method :post :body "x")
               (is (= ["POST" "GET"] (map #(nth % 2) @seen))))
             (testing "five redirects at most, and then the redirect itself"
               (is (= 302 (:status (call "/loop"))))
               (is (= 6 (count @seen))))
             (testing "as many as :max-redirects says"
               (is (= [302 nil] ((juxt :status :redirects) (call "/for-now" :max-redirects 0))))
               (is (= 1 (count @seen)))
               (is (= 302 (:status (call "/then-for-now" :max-redirects 1))))
               (is (= 2 (count @seen))))))))))

#?(:clj
   (deftest a-body-sent-too-slowly
     (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
       (.createContext server "/"
                       (reify HttpHandler
                         (handle [_ exchange]
                           (.sendResponseHeaders exchange 200 0)
                           (let [out (.getResponseBody exchange)]
                             (try
                               (dotimes [_ 50]
                                 (.write out (.getBytes " "))
                                 (.flush out)
                                 (Thread/sleep 100))
                               (catch java.io.IOException _)
                               (finally
                                 (try (.close out) (catch java.io.IOException _))))))))
       (.start server)
       (try
         (let [started (System/currentTimeMillis)]
           (is (= ::http/timeout
                  (try (http/fetch! (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/")
                                    (assoc local :timeout-seconds 1))
                       nil
                       (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
           (is (< (- (System/currentTimeMillis) started) 3000) "given up at the deadline, not when the body ends"))
         (finally
           (.stop server 0))))))

#?(:clj
   (deftest bytes-of-a-media-file
     (let [b (byte-array (map unchecked-byte (range 1000)))]
       (with-file-server b {}
         (fn [url]
           (let [{:keys [status body content-range]} (http/send! {:url url :as :bytes :range [10 19]})]
             (is (= 206 status))
             (is (= {:start 10 :end 19 :total 1000} content-range))
             (is (= (range 10 20) (map #(bit-and % 0xFF) body))))
           (is (= 990 (count (:body (http/send! {:url url :as :bytes :range [10 nil]})))) "to the end")
           (testing "the digest of the whole file, streamed through"
             (is (= (seq (.digest (MessageDigest/getInstance "SHA-384") b))
                    (seq (get-in (http/digest! url ["SHA-384"] local) [:digests "SHA-384"])))))))
       (with-file-server b {:ranges? false}
         (fn [url]
           (let [{:keys [status body content-range]} (http/send! {:url url :as :bytes :range [10 19]})]
             (is (= 200 status) "a server that sends the whole file instead")
             (is (nil? content-range))
             (is (= (range 20) (map #(bit-and % 0xFF) body)) "of which the start up to the range is read"))
           (is (= 1000 (count (:body (http/send! {:url url :as :bytes :range [10 nil]})))) "all of it for the rest"))))))

#?(:clj
   (deftest a-body-into-a-sink
     (let [b      (byte-array (map unchecked-byte (range 200000)))
           pieces (atom [])
           sink   #(swap! pieces conj %)
           joined #(byte-array (mapcat seq @pieces))]
       (with-server {"/file" [200 {} b]
                     "/gz"   [200 {"Content-Encoding" "gzip"} (gzip b)]
                     "/gone" [404 {} (.getBytes "nope" "UTF-8")]}
         (fn [base]
           (testing "the body in pieces, of which the response holds none"
             (let [{:keys [status body received]} (http/send! {:url (str base "/file") :sink sink})]
               (is (= [200 nil 200000] [status body received]))
               (is (< 1 (count @pieces)))
               (is (= (seq b) (seq (joined))))))
           (testing "into an OutputStream"
             (let [out (ByteArrayOutputStream.)]
               (is (= 200000 (:received (http/send! {:url (str base "/file") :sink out}))))
               (is (= (seq b) (seq (.toByteArray out))))))
           (testing "with gzip undone"
             (reset! pieces [])
             (is (= 200000 (:received (http/send! {:url (str base "/gz") :sink sink}))))
             (is (= (seq b) (seq (joined)))))
           (testing "within the limit of the body"
             (is (= ::http/too-large
                    (try (http/send! {:url (str base "/file") :sink sink :max-bytes 1000}) nil
                         (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
             (is (= 200000 (:received (http/send! {:url (str base "/file") :sink sink :max-bytes nil})))
                 "or without one"))
           (testing "but the body of a failure in the response"
             (reset! pieces [])
             (is (= [404 "nope" nil] ((juxt :status :body :received) (http/send! {:url (str base "/gone") :sink sink}))))
             (is (empty? @pieces)))))
       (with-file-server b {}
         (fn [url]
           (reset! pieces [])
           (is (= 100 (:received (http/send! {:url url :range [1000 1099] :sink sink}))))
           (is (= (seq (java.util.Arrays/copyOfRange b 1000 1100)) (seq (joined))) "no more than the range"))))))

#?(:clj
   (deftest requests-that-reach-no-server
     (let [kind   #(try (%) nil (catch clojure.lang.ExceptionInfo e (http/failure e)))
           ;; a server that answers anything in plain HTTP, as most do
           server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getByName "127.0.0.1"))
           answer (future (with-open [s (.accept server)]
                            (doto (.getOutputStream s)
                              (.write (.getBytes "HTTP/1.1 400 Bad Request\r\n\r\n" "UTF-8"))
                              (.flush))
                            (Thread/sleep 500)))]
       (try
         (let [tls (kind #(http/send! {:url (str "https://127.0.0.1:" (.getLocalPort server) "/")
                                       :allow-pred (constantly true)}))]
           (is (= ::http/tls (:type tls)) "a secure connection to a server that doesn't speak TLS")
           (is (re-find #"^The secure connection to https://127\.0\.0\.1:\d+/ failed: " (:reason tls))))
         (finally
           (future-cancel answer)
           (.close server)))
       (is (= {:type ::http/unreachable :url "http://127.0.0.1:1/" :reason "The host of http://127.0.0.1:1/ could not be reached"}
              (kind #(http/send! {:url "http://127.0.0.1:1/" :allow-pred (constantly true)})))
           "a port where nothing listens")
       (is (= ::http/unresolved (:type (kind #(http/send! {:url "http://no-such-host.invalid/" :allow-pred (constantly true)}))))
           "a name that doesn't resolve")
       (is (= ::http/unresolved (:type (kind #(http/fetch! "http://no-such-host.invalid/"))))
           "also when url/public? refuses it, as it refuses a name that doesn't resolve"))))

#?(:clj
   (deftest probing-a-file
     (let [b (byte-array (map unchecked-byte (range 1000)))]
       (with-file-server b {}
         (fn [url]
           (let [{:keys [head get]} (http/probe! url local)]
             (is (= [200 1000] ((juxt :status :length) head)))
             (is (= [206 {:start 0 :end 1 :total 1000}] ((juxt :status :range) get))))))
       (with-file-server b {:ranges? false}
         (fn [url]
           (is (= [200 1000] ((juxt :status :length) (:get (http/probe! url local))))
               "a server that sends the whole file, of which two bytes are read"))))
     (let [closed (with-open [s (java.net.ServerSocket. 0)] (.getLocalPort s))
           probed (http/probe! (str "http://127.0.0.1:" closed "/nothing") local)]
       (is (every? (comp string? :reason :error) (vals probed))
           "a port where no server listens, told by the reason")
       (is (not-any? (comp #{::http/refused} :type :error) (vals probed))))))

#?(:clj
   (deftest more-than-was-asked
     (let [b (byte-array (map unchecked-byte (range 1000)))]
       (with-server {"/a.mp3" [206 {"Content-Range" "bytes 0-999/1000"} b]}
         (fn [base]
           (let [{:keys [status body content-range]} (http/send! {:url (str base "/a.mp3") :as :bytes :range [0 1]})]
             (is (= [206 {:start 0 :end 999 :total 1000}] [status content-range]))
             (is (= [0 1] (map #(bit-and % 0xFF) body)) "a 206 of the whole file, of which the range is read")))))
     (testing "an answer that holds none of the file"
       (let [page (byte-array (* 2 1024 1024) (byte 120))]
         (with-server {"/gone.mp3" [404 {"Content-Type" "text/html"} page]}
           (fn [base]
             (let [{:keys [status body]} (http/send! {:url (str base "/gone.mp3") :range [0 1] :max-bytes 1024})]
               (is (= [404 "xx"] [status body]) "of a page of 2 MB, the two bytes that the range names")
               (is (= 404 (get-in (http/probe! (str base "/gone.mp3") (assoc local :max-bytes 1024))
                                  [:get :status]))))))))))

#?(:clj
   (deftest an-algorithm-that-the-platform-lacks
     (let [asked (atom 0)]
       (with-server {"/a.mp3" [200 {} (byte-array 10)]}
         (fn [base]
           (is (thrown? java.security.NoSuchAlgorithmException
                        (http/digest! (str base "/a.mp3") ["SHA-9000"] {:allow-pred (fn [_] (swap! asked inc) true)})))
           (is (zero? @asked) "no request is sent"))))))

(deftest the-requests-of-a-fetcher
  (let [sent  (atom nil)
        send  (fn [request]
                (reset! sent request)
                {:status 206 :url (:url request) :headers {} :content-range {:start 5 :end 14 :total 1000}})
        fetch (http/fetcher "https://example.org/a.mp3"
                            {:send send :headers {"X-App" "a"} :timeout-seconds 5 :max-bytes 1000})]
    (run #(async/then (fetch 5 15)
                      (fn [{:keys [start total]}]
                        (is (= [5 1000] [start total]))
                        (is (= {:url     "https://example.org/a.mp3"
                                :headers {"X-App" "a"}
                                :as      :bytes
                                :range   [5 14]
                                :timeout-seconds 5
                                :max-bytes       1000
                                :allow-pred      url/public?}
                               @sent)
                            "the limits of time and size, which each request carries"))))))

#?(:clj
   (deftest fetching-past-the-end
     (with-file-server (byte-array 1000) {}
       (fn [url]
         (let [{:keys [bytes start total]} ((http/fetcher url local) 2000 2100)]
           (is (= [0 2000 1000] [(alength ^bytes bytes) start total])
               "a 416 gives no bytes and the size of the file"))))))

(deftest a-fetcher-without-options
  (is (fn? (http/fetcher "https://example.org/a.mp3"))))

(deftest a-fetcher-of-a-file-that-is-gone
  (let [url   "https://example.org/a.mp3"
        fetch (http/fetcher url {:send (fn [request] {:status 404 :url (:url request) :headers {}})})]
    (run #(async/then (async/attempt (fn [] (fetch 0 10)))
                      (fn [{:keys [error]}]
                        (is (= {:type ::http/failed :status 404 :url url} (ex-data error))
                            "a call throws, with the type and the status"))))))

#?(:clj
   (deftest refusing-hosts
     (let [seen (atom [])
           kind #(try (%) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
       (with-servers seen
         (fn [[_ other] path ^HttpExchange exchange]
           (if (= "/away" path)
             (redirect exchange 302 (str other "/feed"))
             (do (.sendResponseHeaders exchange 200 2)
                 (with-open [out (.getResponseBody exchange)]
                   (.write out (.getBytes "ok"))))))
         (fn [[base _]]
           (testing "a redirect that `:allow-pred` refuses is not followed"
             (is (= ::http/refused (kind #(http/send! {:url (str base "/away") :allow-pred (fn [url] (.startsWith ^String url base))}))))
             (is (= 1 (count @seen)) "the first request alone"))
           (testing "`send-public!` sends nothing to the network of the app"
             (reset! seen [])
             (is (= ::http/refused (kind #(http/send-public! {:url (str base "/feed")}))))
             (is (empty? @seen))))))))

#?(:cljs
   (deftest refusing-hosts-in-a-browser
     (async done
       (-> (http/send-public! {:url "http://127.0.0.1:1/feed"})
           (.then (fn [_] (is false "sent")))
           (.catch (fn [e] (is (= ::http/refused (:type (ex-data e))))))
           (.finally done)))))
