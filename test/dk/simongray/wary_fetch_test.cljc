(ns dk.simongray.wary-fetch-test
  "Sending: requests and their credentials, fetch!, the fetcher, probe!
  and the facts of an answer, on both platforms, and on the JVM against
  servers on localhost."
  (:require #?(:clj [clojure.string :as str])
            [clojure.test :refer [deftest is testing #?(:cljs async)]]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.async :as async]
            #?(:cljs [dk.simongray.wary-fetch.browser :as browser])
            [dk.simongray.wary-fetch.limits :as limits]
            [dk.simongray.wary-fetch.redirect :as redirect]
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.wary-fetch.test-support :refer [run #?(:clj with-file-server)]]
            [dk.simongray.wary-fetch.url :as url]
            [dk.simongray.webcrypto.bytes :as bytes])
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

(def token
  "A token as slurp reads it from a file, with a line break at its end."
  "s3cr3t-71a8c14c\n")

(defn- shows-token?
  "Whether the text `s` has the token, or a part of it."
  [s]
  (boolean (re-find #"s3cr3t|71a8|c14c" (str s))))

(defn- check-hidden
  "Check that the `error` of a request with the token in its header
  `header-name` names the header but shows no part of the token."
  [error header-name]
  (is (= {:type ::http/invalid-header :header header-name} (select-keys (ex-data error) [:type :header])))
  (doseq [[way written] {"the message" (ex-message error)
                         "the data"    (pr-str (ex-data error))
                         "the causes"  (async/explain error)
                         "the failure" (pr-str (http/failure error))}]
    (is (not (shows-token? written)) (str header-name ": " way))))

(deftest a-header-that-cant-be-sent
  (testing "a header holds tabs, spaces, visible characters and Latin-1, but no control characters"
    (is (nil? (limits/unsendable-header {"Accept" "*/*" "X-Text" "a\tb øÿ " "X-Empty" ""})))
    (doseq [[what v] {"a line break"               "a\nb"
                      "a carriage return"          "a\r"
                      "a NUL"                      "\u0000"
                      "another control character"  "a\u0001"
                      "DEL"                        "a\u007f"
                      "a character beyond Latin-1" "a€"}]
      (is (= "X-Text" (limits/unsendable-header {"Accept" "*/*" "X-Text" v})) what)))
  (let [url     "http://127.0.0.1:1/"
        request (fn [headers] {:url url :headers headers :allow-pred (constantly true)})
        failed  (fn [send header-name]
                  #(async/then (async/attempt send)
                               (fn [{:keys [error]}] (check-hidden error header-name))))]
    (run (failed #(http/send! (request {"Authorization" (secret/hide (str "Bearer " token))})) "Authorization")
         (failed #(http/fetch! url {:credentials {:token token} :allow-pred (constantly true)}) "Authorization")
         (failed #(http/send! (request {"Cookie" (str "id=" token "X-Injected: 1")})) "Cookie")
         (failed #(http/send! (request {"X-Api-Key" (str token "€")})) "X-Api-Key")
         #?@(:clj  [(failed #(http/digest! url ["SHA-256"] {:headers {"Authorization" (secret/hide token)} :allow-pred (constantly true)})
                            "Authorization")]
             ;; Node's fetch throws with a value that has a line break inside
             :cljs [(failed #(browser/send! (http/prepared (request {"Cookie" (str "id=" token "X-Injected: 1")})) 5 (constantly true))
                            "Cookie")]))))

(deftest a-body-that-cant-be-sent
  (let [url     "http://127.0.0.1:1/"
        request (fn [body] {:method :post :url url :body body :allow-pred (constantly true)})
        failed  (fn [send what]
                  #(async/then (async/attempt send)
                               (fn [{:keys [error]}]
                                 (is (= {:type ::http/invalid-body :url url} (ex-data error)) what))))]
    (run (failed #(http/send! (request {:title "Café"})) "a map, not its printed text")
         (failed #(http/send! (request ["a" "b"])) "another collection")
         #?(:cljs (failed #(browser/send! (http/prepared (request {:title "Café"})) 5 (constantly true))
                          "with fetch too")))))

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

(deftest forms-and-json-to-post
  (let [form (http/form-request "https://auth.example/token" [["code" "a b"] ["client_id" "https://app.example/"]]
                                {:max-redirects 0 :headers {"Accept" "application/json"}})]
    (is (= {:method        :post
            :url           "https://auth.example/token"
            :headers       {"Content-Type" "application/x-www-form-urlencoded" "Accept" "application/json"}
            :max-redirects 0}
           (dissoc form :body)))
    (is (= "#<secret>" (pr-str (:body form))) "a body that holds a secret, which doesn't print")
    (is (= "code=a%20b&client_id=https%3A%2F%2Fapp.example%2F" (:body (http/revealed form)))))
  (is (= {:method :post :url "https://mp.example/" :headers {"Content-Type" "application/json"}
          :body "{\"type\":[\"h-entry\"]}"}
         (http/json-request "https://mp.example/" {:type ["h-entry"]}))))

(deftest files-to-post
  (let [request  (http/multipart-request "https://mp.example/media"
                                         [["h" "entry"]
                                          ["photo" {:content      (bytes/of [0xff 0xd8])
                                                    :filename     "sun\"set.jpg"
                                                    :content-type "image/jpeg"}]
                                          ["note" {:content "Blåbær"}]
                                          ["empty" nil]]
                                         {:max-redirects 0})
        boundary (second (re-find #"^multipart/form-data; boundary=(.+)$"
                                  (get-in request [:headers "Content-Type"])))
        text     #(str "--" boundary "\r\n" (apply str %&) "\r\n")]
    (is (= {:method :post :url "https://mp.example/media" :max-redirects 0}
           (dissoc request :headers :body)))
    (is (= "#<secret>" (pr-str (:body request))) "a body that doesn't print")
    (is (= (bytes/hex (bytes/concat
                       (bytes/encode (text "Content-Disposition: form-data; name=\"h\"\r\n\r\nentry"))
                       (bytes/encode (str "--" boundary "\r\n"
                                          "Content-Disposition: form-data; name=\"photo\"; filename=\"sun%22set.jpg\"\r\n"
                                          "Content-Type: image/jpeg\r\n\r\n"))
                       (bytes/of [0xff 0xd8])
                       (bytes/encode (str "\r\n"
                                          (text "Content-Disposition: form-data; name=\"note\"; filename=\"blob\"\r\n"
                                                "Content-Type: application/octet-stream\r\n\r\nBlåbær")
                                          "--" boundary "--\r\n"))))
           (bytes/hex (:body (http/revealed request))))
        "a text, a file of bytes, and one of text, with the defaults of a browser")
    (is (not= boundary (get-in (http/multipart-request "https://mp.example/media" {"h" "entry"})
                               [:headers "Content-Type"]))
        "a boundary of its own")))

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
   (deftest parameters-of-the-query
     (with-server {"/search" [200 {} (.getBytes "ok" "UTF-8")]}
       (fn [base]
         (is (= (str base "/search?lang=da&q=a%20b")
                (:url (http/send! (merge local {:url          (str base "/search?lang=da")
                                                :query-params {:q "a b" :page nil}}))))
             "after the parameters of the URL, without a nil one")))))

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
     (is (str/includes? (slurp "build.clj") (pr-str http/user-agent))
         "the User-Agent that the jar names with the version of its tag")))

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
             (testing "a :prepare-fn gives the request of each hop, after the credentials stay behind"
               (call "/away" :prepare-fn #(assoc-in % [:headers "Authorization"] (str "Hop " (:url %))))
               (is (= [(str "Hop " base "/away") (str "Hop " other "/feed")] (map last @seen)))
               (call "/for-now" :prepare-fn #(future (assoc-in % [:headers "Authorization"] "Later")))
               (is (= ["Later" "Later"] (map last @seen)) "or a future of it"))
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
   (deftest a-request-that-its-signal-stops
     (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
           slow   (fn [headers-first?]
                    (reify HttpHandler
                      (handle [_ exchange]
                        (when-not headers-first?
                          (Thread/sleep 3000))
                        (.sendResponseHeaders exchange 200 0)
                        (let [out (.getResponseBody exchange)]
                          (try
                            (dotimes [_ 50]
                              (.write out (.getBytes " "))
                              (.flush out)
                              (Thread/sleep 100))
                            (catch java.io.IOException _)
                            (finally
                              (try (.close out) (catch java.io.IOException _))))))))]
       (.createContext server "/body" (slow true))
       (.createContext server "/headers" (slow false))
       (.start server)
       (try
         (doseq [path ["/body" "/headers"]]
           (let [signal  (atom false)
                 base    (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                 started (System/currentTimeMillis)]
             (future (Thread/sleep 300) (reset! signal true))
             (let [e (try (http/fetch! (str base path) (assoc local :signal signal))
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
               (is (= ::http/aborted (:type (ex-data e))) path)
               (is (http/aborted? e)))
             (is (< (- (System/currentTimeMillis) started) 1500) (str "stopped at once, while " path " came"))))
         (is (http/aborted? (try (http/fetch! "http://127.0.0.1:1/" (assoc local :signal (doto (promise) (deliver true))))
                                 nil
                                 (catch clojure.lang.ExceptionInfo e e)))
             "or before it went out, by a promise that is delivered")
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
   (deftest a-body-cut-at-the-limit
     (let [page  (apply str (repeat 3000 "abcdefghij"))
           bytes (.getBytes page "UTF-8")
           send  #(http/send! (merge {:allow-pred (constantly true) :max-bytes 100 :truncate? true} %))]
       (with-server {"/page" [200 {} bytes]
                     "/gz"   [200 {"Content-Encoding" "gzip"} (gzip bytes)]
                     "/json" [200 {"Content-Type" "application/json"} (.getBytes "{\"a\":[1,2]}" "UTF-8")]}
         (fn [base]
           (is (= [(subs page 0 100) true] ((juxt :body :truncated?) (send {:url (str base "/page")})))
               "the start of a body larger than :max-bytes, rather than a throw")
           (is (= [(subs page 0 100) true] ((juxt :body :truncated?) (send {:url (str base "/gz")})))
               "gzip undone first")
           (is (= [30000 nil] ((juxt (comp count :body) :truncated?) (send {:url (str base "/page") :max-bytes 30000})))
               "and a body that fits whole is no cut")
           (is (= {:a [1 2]} (:body (send {:url (str base "/json") :as :json}))) "JSON read")
           (is (nil? (:body (send {:url (str base "/page") :as :json :max-bytes nil}))) "and nil when it isn't JSON"))))))

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
           ;; a server that answers anything in plain HTTP, as most do. It
           ;; reads before it answers, and waits for the client to hang up,
           ;; since a close with unread data resets the connection, and on
           ;; Linux the reset can drop the answer before the client reads it
           server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getByName "127.0.0.1"))
           answer (future (with-open [s (.accept server)]
                            (let [in (.getInputStream s)]
                              (.setSoTimeout s 5000)
                              (.read in)
                              (doto (.getOutputStream s)
                                (.write (.getBytes "HTTP/1.1 400 Bad Request\r\n\r\n" "UTF-8"))
                                (.flush))
                              (try
                                (while (not= -1 (.read in (byte-array 4096))))
                                (catch java.io.IOException _)))))]
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
