(ns dk.simongray.wary-fetch.node-test
  "send! in Node, with servers of node:http on localhost: bodies, limits
  and the User-Agent, redirects and the checks of each, the addresses that
  the :allow-pred sees, and the time that an exchange may take."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.test-support :refer [run with-node-server]]
            [dk.simongray.wary-fetch.url :as url]))

(defn- answer
  "Answer `res` with `status`, the `headers` and the `body`."
  ([^js res status]
   (answer res status {} nil))
  ([^js res status headers body]
   (.writeHead res status (clj->js headers))
   (.end res body)))

(defn- failure-type
  "The :type of the exception of the attempt to send `request`."
  [request]
  (.then (http/try-send! http/send! request)
         (fn [{:keys [error]}] (:type (ex-data error)))))

(deftest bodies-in-node
  (run
    #(with-node-server
       (fn [^js req res]
         (case (.-url req)
           "/feed.xml" (answer res 200 {"Content-Type" "application/rss+xml; charset=ISO-8859-1"
                                        "ETag"         "\"v1\""}
                               (js/Buffer.from "<rss>Blåbær</rss>" "latin1"))
           "/gzip"     (answer res 200 {"Content-Encoding" "gzip"}
                               (.gzipSync (js/require "node:zlib") "hello"))
           "/bomb"     (answer res 200 {"Content-Encoding" "gzip"}
                               (.gzipSync (js/require "node:zlib") (js/Buffer.alloc 100000)))
           "/big"      (answer res 200 {} (js/Buffer.alloc 5000))
           "/whole"    (answer res 200 {} (js/Buffer.alloc 100000 1))
           "/agent"    (answer res 200 {} (str (aget (.-headers req) "user-agent") " "
                                               (aget (.-headers req) "accept-encoding")))
           (answer res 404 {} "nope")))
       (fn [base]
         (-> (http/send! {:url (str base "/feed.xml")})
             (.then (fn [{:keys [status body headers]}]
                      (is (= [200 "<rss>Blåbær</rss>"] [status body]) "decoded by the charset of the Content-Type")
                      (is (= ["\"v1\""] (get headers "etag")))
                      (http/send! {:url (str base "/gzip")})))
             (.then (fn [response]
                      (is (= "hello" (:body response)) "gzip undone")
                      (failure-type {:url (str base "/big") :max-bytes 1000})))
             (.then (fn [type]
                      (is (= ::http/too-large type))
                      (failure-type {:url (str base "/bomb") :max-bytes 1000})))
             (.then (fn [type]
                      (is (= ::http/too-large type) "the limit counts the bytes after gzip is undone")
                      (http/send! {:url (str base "/agent") :user-agent "Mine"})))
             (.then (fn [response]
                      (is (= "Mine gzip" (:body response)))
                      (http/send! {:url (str base "/agent") :range [0 99]})))
             (.then (fn [response]
                      (is (str/ends-with? (:body response) " identity") "a range of bytes as they are")
                      (http/send! {:url (str base "/whole") :range [0 9] :as :bytes})))
             (.then (fn [response]
                      (is (= [200 10] [(:status response) (.-length (:body response))])
                          "of a server that ignores ranges, no more than the range")
                      (http/send! {:url (str base "/gone") :method :head})))
             (.then (fn [response]
                      (is (= [404 nil] ((juxt :status :body) response)) "no body for a HEAD"))))))))

(deftest redirects-in-node
  (let [seen (atom [])]
    (run
      #(with-node-server
         (fn [^js req res]
           (swap! seen conj [(.-url req) (aget (.-headers req) "authorization")])
           (case (.-url req)
             "/moved"       (answer res 301 {"Location" "/moved-again"} nil)
             "/moved-again" (answer res 308 {"Location" "/feed"} nil)
             "/for-now"     (answer res 302 {"Location" "/feed"} nil)
             "/away"        (answer res 302 {"Location" (str "http://localhost:"
                                                             (.. req -socket -localPort)
                                                             "/feed")}
                                    nil)
             "/elsewhere"   (answer res 302 {"Location" "ftp://files.example/feed"} nil)
             "/loop"        (answer res 302 {"Location" "/loop"} nil)
             (answer res 200 {} "ok")))
         (fn [base]
           (-> (http/send! {:url (str base "/moved")})
               (.then (fn [{:keys [status url permanent-url redirects]}]
                        (is (= [200 (str base "/feed") (str base "/feed")] [status url permanent-url]))
                        (is (= [[301 (str base "/moved")] [308 (str base "/moved-again")]]
                               (map (juxt first second) redirects))
                            "each redirect, with the URL that answered with it")
                        (http/send! {:url (str base "/for-now")})))
               (.then (fn [response]
                        (is (= 200 (:status response)))
                        (is (nil? (:permanent-url response)) "a temporary redirect moves nothing for good")
                        (reset! seen [])
                        (http/send! {:url (str base "/away") :headers {"Authorization" "Basic dTpw"}})))
               (.then (fn [response]
                        (is (= 200 (:status response)))
                        (is (= [["/away" "Basic dTpw"] ["/feed" nil]] @seen)
                            "the credentials stay behind when a redirect leaves the origin")
                        (http/send! {:url (str base "/elsewhere")})))
               (.then (fn [response]
                        (is (= 302 (:status response)) "a redirect to a URL that isn't http or https is not followed")
                        (http/send! {:url (str base "/loop")})))
               (.then (fn [response]
                        (is (= [302 5] [(:status response) (count (:redirects response))]) "five redirects at most")
                        (reset! seen [])
                        (http/send! {:url (str base "/for-now") :max-redirects 0})))
               (.then (fn [response]
                        (is (= [302 nil] [(:status response) (:redirects response)]))
                        (is (= 1 (count @seen)) "none with :max-redirects 0")
                        (failure-type {:url        (str base "/for-now")
                                       :allow-pred (fn [u] (not (str/ends-with? u "/feed")))})))
               (.then (fn [type]
                        (is (= ::http/refused type) ":allow-pred takes the URL of each redirect")))))))))

(deftest addresses-in-node
  (let [asked (atom nil)]
    (run
      #(with-node-server
         (fn [_ res] (answer res 200 {} "ok"))
         (fn [base]
           (-> (http/send! {:url        (str/replace base "127.0.0.1" "localhost")
                            :allow-pred (fn [_] (reset! asked url/*addresses*) true)})
               (.then (fn [response]
                        (is (= 200 (:status response)))
                        (is (some #{[127 0 0 1]} (get @asked "localhost"))
                            "the :allow-pred sees the addresses that the name resolves to")
                        (failure-type {:url (str base "/") :allow-pred url/public?})))
               (.then (fn [type]
                        (is (= ::http/refused type) "an address of loopback is no public one")))))))))

(deftest urls-in-node
  (run
    #(with-node-server
       (fn [^js req res] (answer res 200 {} (.-url req)))
       (fn [base]
         ;; Node 24 encodes ^ in a path only when something else needs it
         (.then (http/send! {:url (str base "/a^b")})
                (fn [response]
                  (is (= "/a%5Eb" (:body response))
                      "the URL as whatwg-url writes it, rather than as Node's own parser reads it")))))))

(deftest time-in-node
  (run
    #(with-node-server
       (fn [_ ^js res]
         (.writeHead res 200)
         (.write res "the start of a body that never ends"))
       (fn [base]
         (.then (http/try-send! http/send! {:url (str base "/") :timeout-seconds 1})
                (fn [{:keys [error]}]
                  (is (= ::http/timeout (:type (ex-data error))))
                  (is (http/timeout? error))))))))

(deftest a-body-into-a-sink-in-node
  (let [b       (js/Buffer.alloc 200000 7)
        pieces  (atom [])
        sink    #(swap! pieces conj %)
        total   #(reduce + (map (fn [^js p] (.-length p)) @pieces))
        slow    (fn [p]
                  (swap! pieces conj p)
                  (js/Promise. (fn [ok] (js/setTimeout ok 1))))
        written (atom 0)
        ^js out (new (.-Writable (js/require "node:stream"))
                     #js {:highWaterMark 1024
                          :write         (fn [^js chunk _ done]
                                           (swap! written + (.-length chunk))
                                           (js/setTimeout done 1))})]
    (run
      #(with-node-server
         (fn [^js req res]
           (case (.-url req)
             "/file" (answer res 200 {} b)
             "/gz"   (answer res 200 {"Content-Encoding" "gzip"} (.gzipSync (js/require "node:zlib") b))
             (answer res 404 {} "nope")))
         (fn [base]
           (-> (http/send! {:url (str base "/file") :sink sink})
               (.then (fn [{:keys [status body received]}]
                        (is (= [200 nil 200000 200000] [status body received (total)])
                            "the body in pieces, of which the response holds none")
                        (reset! pieces [])
                        (http/send! {:url (str base "/file") :sink slow})))
               (.then (fn [{:keys [received]}]
                        (is (= [200000 200000] [received (total)]) "a sink that gives a promise to wait for")
                        (http/send! {:url (str base "/file") :sink out})))
               (.then (fn [{:keys [received]}]
                        (is (= [200000 200000] [received @written]) "into a writable stream, as fast as it drains")
                        (reset! pieces [])
                        (http/send! {:url (str base "/gz") :sink sink})))
               (.then (fn [{:keys [received]}]
                        (is (= [200000 200000] [received (total)]) "with gzip undone")
                        (reset! pieces [])
                        (http/send! {:url (str base "/file") :sink sink :range [0 99]})))
               (.then (fn [{:keys [received]}]
                        (is (= [100 100] [received (total)]) "no more than the range")
                        (failure-type {:url (str base "/file") :sink sink :max-bytes 1000})))
               (.then (fn [type]
                        (is (= ::http/too-large type) "within the limit of the body")
                        (reset! pieces [])
                        (http/send! {:url (str base "/gone") :sink sink})))
               (.then (fn [response]
                        (is (= [404 "nope" nil 0] [(:status response) (:body response) (:received response) (total)])
                            "but the body of a failure in the response")))))))))

(deftest a-body-cut-at-the-limit-in-node
  (let [page (apply str (repeat 3000 "abcdefghij"))
        send #(http/send! (merge {:allow-pred (constantly true) :max-bytes 100 :truncate? true} %))]
    (run
      #(with-node-server
         (fn [^js req res]
           (case (.-url req)
             "/page" (answer res 200 {} page)
             "/gz"   (answer res 200 {"Content-Encoding" "gzip"} (.gzipSync (js/require "node:zlib") page))
             "/json" (answer res 200 {"Content-Type" "application/json"} "{\"a\":[1,2]}")))
         (fn [base]
           (-> (send {:url (str base "/page")})
               (.then (fn [r]
                        (is (= [(subs page 0 100) true] ((juxt :body :truncated?) r))
                            "the start of a body larger than :max-bytes, rather than a throw")
                        (send {:url (str base "/gz")})))
               (.then (fn [r]
                        (is (= [(subs page 0 100) true] ((juxt :body :truncated?) r)) "gzip undone first")
                        (send {:url (str base "/page") :max-bytes 30000})))
               (.then (fn [r]
                        (is (= [30000 nil] ((juxt (comp count :body) :truncated?) r)) "and a body that fits is no cut")
                        (send {:url (str base "/json") :as :json})))
               (.then (fn [r]
                        (is (= {:a [1 2]} (:body r)) "JSON read")))))))))

(deftest requests-that-reach-no-server-in-node
  (let [failure (fn [request]
                  (.then (http/try-send! http/send! (merge {:allow-pred (constantly true)} request))
                         (fn [{:keys [error]}] (http/failure error))))]
    (run
      #(with-node-server
         (fn [_ res] (answer res 200 {} "plain"))
         (fn [base]
           (.then (failure {:url (str/replace base "http:" "https:")})
                  (fn [{:keys [type reason]}]
                    (is (= ::http/tls type) "a secure connection to a server that doesn't speak TLS")
                    (is (str/starts-with? reason "The secure connection to https://127.0.0.1:"))))))
      #(.then (failure {:url "http://127.0.0.1:1/"})
              (fn [f]
                (is (= {:type ::http/unreachable :url "http://127.0.0.1:1/"
                        :reason "The host of http://127.0.0.1:1/ could not be reached"}
                       f)
                    "a port where nothing listens")))
      #(.then (failure {:url "http://no-such-host.invalid/"})
              (fn [{:keys [type]}]
                (is (= ::http/unresolved type) "a name that doesn't resolve")))
      #(.then (http/try-send! http/send-public! {:url "http://no-such-host.invalid/"})
              (fn [{:keys [error]}]
                (is (= ::http/unresolved (:type (ex-data error))) "also for a public request"))))))
