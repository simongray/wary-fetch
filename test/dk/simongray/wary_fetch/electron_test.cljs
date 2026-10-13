(ns dk.simongray.wary-fetch.electron-test
  "The sender of an Electron page and the handler of its main process,
  over a stand-in for the IPC that clones what crosses it as Electron
  does, so that nothing but plain data can cross."
  (:require [clojure.test :refer [deftest is]]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.bytes :as bytes]
            [dk.simongray.wary-fetch.electron :as electron]
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.wary-fetch.test-support :refer [run with-node-server]]
            [dk.simongray.wary-fetch.url :as url]))

(defn- ipc
  "The invoke of a page whose requests the `handler` of the main process
  answers, with each crossing cloned by the structured clone algorithm."
  [handler]
  (fn [o]
    (-> (js/Promise.resolve (handler nil (js/structuredClone o)))
        (.then #(js/structuredClone %)))))

(deftest a-page-through-the-main-process
  (let [sent    (atom [])
        answer  {:status        206
                 :url           "https://b.example/feed.json"
                 :headers       {"content-type" ["application/json"]}
                 :body          "{\"title\":\"Café\"}"
                 :content-range {:start 0 :end 15 :total 100}
                 :redirects     [[301 "https://a.example/feed.json" {"location" ["https://b.example/feed.json"]}]]
                 :permanent-url "https://b.example/feed.json"}
        main    (electron/handler {:send       (fn [request] (swap! sent conj request) answer)
                                   :max-bytes  1000
                                   :allow-pred url/public?})
        send    (electron/sender (ipc main))]
    (run
     #(.then (send {:method     :get
                    :url        "https://a.example/feed.json"
                    :headers    {"Authorization" (secret/hide "Bearer t")}
                    :as         :json
                    :range      [0 15]
                    :max-bytes  1000000
                    :allow-pred (constantly true)
                    :sink       (fn [_])})
             (fn [response]
               (is (= (assoc answer :body {:title "Café"}) response)
                   "the response as send! gives it, and JSON read in the page")
               (let [request (peek @sent)]
                 (is (= {:method :get :url "https://a.example/feed.json" :headers {"Authorization" "Bearer t"}
                         :range [0 15] :max-bytes 1000 :timeout-seconds 30 :allow-pred url/public?}
                        request)
                     "the limits of the main process, whatever the page asks, and no :sink"))))
     #(.then (send {:url "https://a.example/x" :timeout-seconds 600})
             (fn [_]
               (is (= 30 (:timeout-seconds (peek @sent)))))))))

(deftest bytes-through-the-main-process
  (let [sent (atom [])
        main (electron/handler {:send (fn [request]
                                        (swap! sent conj request)
                                        {:status 200 :url "u" :headers {} :body (bytes/of [1 2 3])})})
        send (electron/sender (ipc main))]
    (run #(.then (send {:method :post :url "https://a.example/x" :body (bytes/of [9]) :as :bytes})
                 (fn [{:keys [body]}]
                   (is (= [1 2 3] (vec body)) "bytes cross as bytes")
                   (is (= [[9] :bytes] ((juxt (comp vec :body) :as) (peek @sent)))))))))

(deftest failures-through-the-main-process
  (let [send (electron/sender (ipc (electron/handler)))]
    (run
     #(.then (http/try-send! send {:url "http://127.0.0.1:9/admin"})
             (fn [{:keys [error]}]
               (is (= {:type ::http/refused :url "http://127.0.0.1:9/admin"} (ex-data error))
                   "the :allow-pred of the main process, url/public? by default, and the type of the failure")))
     #(with-node-server
        (fn [_ ^js res] (.writeHead res 200 #js {"Content-Type" "text/plain; charset=utf-8"}) (.end res "hello"))
        (fn [base]
          (let [send (electron/sender (ipc (electron/handler {:allow-pred (constantly true)})))]
            (.then (send {:url (str base "/")})
                   (fn [{:keys [status body]}]
                     (is (= [200 "hello"] [status body])) "sent by send! in Node"))))))))
