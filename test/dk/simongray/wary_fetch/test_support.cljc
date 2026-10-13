(ns dk.simongray.wary-fetch.test-support
  "What the tests share: a test of steps that give promises in
  ClojureScript, and on the JVM a server of files on localhost."
  #?(:cljs (:require [clojure.test :refer [is async]]))
  #?(:clj (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
                   [java.net InetSocketAddress]
                   [java.util Arrays])))

(defn run
  "Run the steps `fs` one after another as a test on either platform. Each
  step is a function of no arguments that gives a value or a promise.

  In ClojureScript, a deftest runs only the async test that it returns. So
  a deftest calls `run` once, as its last form, with all its steps."
  [& fs]
  #?(:clj  (doseq [f fs] (f))
     :cljs (async done
             (-> (reduce (fn [p f] (.then p (fn [_] (f)))) (js/Promise.resolve nil) fs)
                 (.catch (fn [e] (is false (str "rejected: " e))))
                 (.finally done)))))

(defn utf8
  "The text of the UTF-8 bytes `b`."
  [b]
  #?(:clj  (String. ^bytes b "UTF-8")
     :cljs (.decode (js/TextDecoder.) b)))

#?(:cljs
   (defn ipc
     "The invoke of an Electron page whose requests the `handler` of the
     main process answers, with each crossing cloned by the structured
     clone algorithm, as Electron clones it."
     [handler]
     (fn [o]
       (-> (js/Promise.resolve (handler nil (js/structuredClone o)))
           (.then #(js/structuredClone %))))))

#?(:cljs
   (defn with-node-server
     "A promise of what `f` gives, as a promise or a value, called with the
     base URL of a server of node:http on 127.0.0.1 that answers each
     request with `(handler req res)`. The server closes when it settles."
     [handler f]
     (let [^js server (.createServer (js/require "node:http") handler)]
       (js/Promise.
        (fn [resolve reject]
          (.listen server 0 "127.0.0.1"
                   (fn []
                     (-> (js/Promise.resolve nil)
                         (.then #(f (str "http://127.0.0.1:"
                                         (.-port (.address server)))))
                         (.finally (fn []
                                     (.closeAllConnections server)
                                     (.close server)))
                         (.then resolve reject)))))))))

#?(:clj
   (defn- answer
     "What the server of `with-server` answers the request for the file
     `file` with `method` and the Range header value `range`: `[status
     bytes headers]`."
     [{:keys [body headers status ranges? head?] :or {status 200 ranges? true head? true} :as file} method range]
     (let [^bytes b (if (string? body) (.getBytes ^String body "UTF-8") (or body (byte-array 0)))
           total    (alength b)
           [_ s e]  (when (and range ranges? (= 200 status) (= "GET" method))
                      (re-matches #"bytes=(\d+)-(\d*)" range))]
       (cond
         (nil? file)
         [404 (byte-array 0) {}]

         (and (= "HEAD" method) (not head?))
         [405 (byte-array 0) {}]

         (and s (>= (parse-long s) total))
         [416 (byte-array 0) (assoc headers "Content-Range" (str "bytes */" total))]

         s
         (let [start (parse-long s)
               end   (min (dec total) (if (seq e) (parse-long e) (dec total)))]
           [206 (Arrays/copyOfRange b (int start) (int (inc end)))
            (assoc headers "Content-Range" (str "bytes " start "-" end "/" total))])

         :else
         [status b headers]))))

#?(:clj
   (defn with-server
     "Call `f` with the URL of a server on localhost that serves `files`, a
     map of paths to `{:body :headers :status :ranges? :head?}`: the bytes or
     the string of the body, the headers of the answer, its status (default
     200), whether a Range request gets the bytes it names, or 416 for a
     range past the end, as RFC 9110 has it (default true), and whether HEAD
     is answered (default true). A path the map lacks is answered 404.
     `log`, when given, is called with each request as `{:method :path
     :range :origin :authorization}`."
     ([files f]
      (with-server files nil f))
     ([files log f]
      (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
        (.createContext server "/"
                        (reify HttpHandler
                          (handle [_ exchange]
                            (let [^HttpExchange exchange exchange
                                  method   (.getRequestMethod exchange)
                                  path     (.getPath (.getRequestURI exchange))
                                  request  (.getRequestHeaders exchange)
                                  range    (.getFirst request "Range")
                                  _        (when log
                                             (log {:method method :path path :range range
                                                   :origin (.getFirst request "Origin")
                                                   :authorization (.getFirst request "Authorization")}))
                                  [status ^bytes part headers] (answer (get files path) method range)
                                  n        (alength part)]
                              (doseq [[k v] headers]
                                (.add (.getResponseHeaders exchange) (str k) (str v)))
                              ;; the JDK server leaves out the length of an answer to HEAD
                              ;; unless it is set by hand, and takes 0 to mean chunked
                              (if (or (= "HEAD" method) (zero? n))
                                (do (.add (.getResponseHeaders exchange) "Content-Length" (str n))
                                    ;; with the request body open, Java 11 closes the connection
                                    ;; after an answer with no body, and the client reuses it
                                    (.close (.getRequestBody exchange))
                                    (.sendResponseHeaders exchange status -1))
                                (do (.sendResponseHeaders exchange status n)
                                    (with-open [out (.getResponseBody exchange)] (.write out part))))
                              (.close exchange)))))
        (.start server)
        (try
          (f (str "http://127.0.0.1:" (.getPort (.getAddress server))))
          (finally
            (.stop server 0)))))))

#?(:clj
   (defn with-file-server
     "Call `f` with the URL of the bytes `b` as a file on a server on
     localhost, as `with-server` serves it, which answers a Range request
     with those bytes unless `:ranges?` in `opts` is false; `:log`, an atom,
     gets the Range header of each request, nil for none."
     [b {:keys [ranges? log] :or {ranges? true}} f]
     (with-server {"/episode" {:body b :ranges? ranges?}}
                  (when log #(swap! log conj (:range %)))
                  #(f (str % "/episode")))))
