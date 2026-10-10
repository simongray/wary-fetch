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
