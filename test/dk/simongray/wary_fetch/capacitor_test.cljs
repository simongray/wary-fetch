(ns dk.simongray.wary-fetch.capacitor-test
  "The sender over CapacitorHttp, through a stand-in for the plugin that
  answers as its native code does: bytes in base64, the value of a JSON
  body, and redirects that it leaves to the sender."
  (:require [clojure.test :refer [deftest is]]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.capacitor :as capacitor]
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.wary-fetch.test-support :refer [run]]
            [dk.simongray.wary-fetch.url :as url]
            [dk.simongray.webcrypto.bytes :as bytes]))

(defn- plugin
  "A stand-in for CapacitorHttp that answers each URL by `answers`, a map
  of URL to an answer, to :offline for a failure of the network, or to
  :never for a server that doesn't answer. Any other URL answers 404. The
  options of each request go into `asked`."
  [answers asked]
  #js {:request
       (fn [options]
         (let [o (js->clj options :keywordize-keys true)
               a (get answers (:url o))]
           (swap! asked conj o)
           (case a
             :offline (js/Promise.reject (js/Error. "The Internet connection appears to be offline."))
             :never   (js/Promise. (fn [_ _]))
             nil      (js/Promise.resolve #js {:status 404 :headers #js {} :data "" :url (:url o)})
             (js/Promise.resolve (clj->js (merge {:url (:url o) :headers {}} a))))))})

(defn- base64
  "The `xs`, numbers from 0 to 255, in base64."
  [xs]
  (bytes/base64 (bytes/of xs)))

(defn- failure-type
  "A promise of the :type of the exception of `send` for `request`."
  [send request]
  (.then (http/try-send! send request)
         (fn [{:keys [error]}] (:type (ex-data error)))))

(deftest bodies-over-capacitor
  (let [asked   (atom [])
        cafe    [67 97 102 233]
        answers {"https://a.example/feed.xml" {:status  200
                                               :headers {"content-type" "application/rss+xml; charset=windows-1252"}
                                               :data    (base64 cafe)}
                 "https://a.example/api"      {:status  200
                                               :headers {"content-type" "application/json; charset=utf-8"}
                                               :data    {:title "Café" :episodes [1 2]}}
                 "https://a.example/bad.json" {:status  500
                                               :headers {"content-type" "application/json"}
                                               :data    "not JSON"}
                 "https://a.example/big"      {:status 200 :headers {} :data (base64 (range 100))}}
        send    (capacitor/sender (plugin answers asked))]
    (run
     #(.then (send {:url "https://a.example/feed.xml" :user-agent "my-app/1.0" :headers {"Accept" "application/rss+xml"}})
             (fn [{:keys [status body]}]
               (is (= [200 "Café"] [status body]) "text by its charset, which the plugin would read as UTF-8")
               (let [o (peek @asked)]
                 (is (= {:method "GET" :responseType "arraybuffer" :disableRedirects true}
                        (select-keys o [:method :responseType :disableRedirects])))
                 (is (= {"User-Agent" "my-app/1.0" "Accept" "application/rss+xml"}
                        (select-keys (update-keys (:headers o) name) ["User-Agent" "Accept"])))
                 (is (<= 29000 (:connectTimeout o) 30000)
                     "the 30 seconds left, less the milliseconds that pass before the request"))))
     #(.then (send {:url "https://a.example/feed.xml" :as :bytes})
             (fn [{:keys [body]}]
               (is (= cafe (vec body)))))
     #(.then (send {:url "https://a.example/api" :as :json})
             (fn [{:keys [body]}]
               (is (= {:title "Café" :episodes [1 2]} body) "a body that the plugin parsed, as JSON again")))
     #(.then (send {:url "https://a.example/bad.json"})
             (fn [{:keys [status body]}]
               (is (= [500 "not JSON"] [status body]) "and its text when it didn't parse")))
     #(.then (send {:url "https://a.example/missing"})
             (fn [{:keys [status body]}]
               (is (= [404 nil] [status body]))))
     #(.then (send {:method :post :url "https://a.example/api" :body (bytes/of [1 2 3])})
             (fn [_]
               (let [o (peek @asked)]
                 (is (= ["POST" "AQID" "file"] ((juxt :method :data :dataType) o))
                     "bytes go as base64, which the plugin decodes for a file")
                 (is (= "application/octet-stream" (get (update-keys (:headers o) name) "Content-Type"))
                     "with a Content-Type, without which the plugin on iOS sends no body"))))
     #(.then (send {:method :post :url "https://a.example/api" :body "{}"})
             (fn [_]
               (let [o (peek @asked)]
                 (is (= ["{}" nil] ((juxt :data :dataType) o)))
                 (is (= "text/plain;charset=UTF-8" (get (update-keys (:headers o) name) "Content-Type"))
                     "text with the type that fetch gives it"))))
     #(.then (send {:method :post :url "https://a.example/api" :body "{}" :headers {"content-type" "application/json"}})
             (fn [_]
               (is (= {"content-type" "application/json"}
                      (select-keys (update-keys (:headers (peek @asked)) name) ["content-type" "Content-Type"]))
                   "and a type that the request names, in any case, stays")))
     #(.then (send {:url "https://a.example/api"})
             (fn [_]
               (is (not-any? #{"Content-Type"} (map name (keys (:headers (peek @asked)))))
                   "a request without a body has no type")))
     #(.then (failure-type send {:url "https://a.example/big" :max-bytes 10})
             (fn [type] (is (= ::http/too-large type))))
     #(.then (send {:url "https://a.example/big" :max-bytes 10 :truncate? true})
             (fn [{:keys [body truncated?]}]
               (is (= [10 true] [(.-length (bytes/encode body)) truncated?]))))
     #(.then (send {:url "https://a.example/big" :as :bytes :sink (fn [b] (swap! asked conj [:sunk (.-length b)]))})
             (fn [{:keys [body received]}]
               (is (= [nil 100 [:sunk 100]] [body received (peek @asked)])))))))

(deftest ranges-over-capacitor
  (let [asked   (atom [])
        whole   (range 200)
        answers {"https://a.example/ranged.mp3" {:status  206
                                                 :headers {"content-range" "bytes 0-9/200"}
                                                 :data    (base64 (take 10 whole))}
                 "https://a.example/whole.mp3"  {:status 200 :headers {} :data (base64 whole)}}
        send    (capacitor/sender (plugin answers asked))]
    (run
     #(.then (send {:url "https://a.example/ranged.mp3" :as :bytes :range [0 9]})
             (fn [{:keys [status body content-range]}]
               (is (= [206 10 {:start 0 :end 9 :total 200}] [status (.-length body) content-range]))
               (is (= {"Range" "bytes=0-9" "Accept-Encoding" "identity"}
                      (select-keys (update-keys (:headers (peek @asked)) name) ["Range" "Accept-Encoding"]))
                   "the offsets of the file, not of gzip")))
     #(.then (send {:url "https://a.example/whole.mp3" :as :bytes :range [0 9]})
             (fn [{:keys [status body]}]
               (is (= [200 (range 10)] [status (vec body)]) "the start of a file from a server that ignores ranges"))))))

(deftest redirects-over-capacitor
  (let [asked   (atom [])
        answers {"https://a.example/old"   {:status 301 :headers {"location" "/new"}}
                 "https://a.example/new"   {:status 302 :headers {"location" "https://b.example/feed"}}
                 "https://b.example/feed"  {:status 200 :headers {"content-type" "text/plain"} :data (base64 [104 105])}
                 "https://a.example/local" {:status 302 :headers {"location" "https://127.0.0.1/admin"}}
                 "https://a.example/down"  {:status 302 :headers {"location" "http://a.example/plain"}}
                 "https://a.example/gone"  :offline
                 "https://a.example/slow"  :never}
        send    (capacitor/sender (plugin answers asked))]
    (run
     #(.then (send {:url "https://a.example/old" :headers {"Authorization" "Bearer t"}})
             (fn [{:keys [url body permanent-url redirects]}]
               (is (= ["https://b.example/feed" "hi" "https://a.example/new"] [url body permanent-url]))
               (is (= [301 302] (map first redirects)))
               (is (nil? (get (update-keys (:headers (peek @asked)) name) "Authorization"))
                   "credentials stay behind on another origin")))
     #(.then (failure-type send {:url "https://a.example/local" :allow-pred url/public?})
             (fn [type] (is (= ::http/refused type) "each URL of a redirect asked of the :allow-pred")))
     #(.then (send {:url "https://a.example/down"})
             (fn [{:keys [status]}] (is (= 302 status) "never from https to http")))
     #(.then (send {:url "https://a.example/old" :max-redirects 0})
             (fn [{:keys [status]}] (is (= 301 status))))
     #(.then (failure-type send {:url "https://a.example/gone"})
             (fn [type] (is (= ::http/unreachable type))))
     #(.then (failure-type send {:url "https://a.example/slow" :timeout-seconds 0.05})
             (fn [type] (is (= ::http/timeout type))))
     #(.then (failure-type send {:url "http://localhost/x" :allow-pred url/public?})
             (fn [type] (is (= ::http/refused type)))))))

(deftest a-header-that-cant-be-sent-over-capacitor
  (let [asked (atom [])
        send  (capacitor/sender (plugin {} asked))]
    (run
     #(.then (http/try-send! send {:url "https://a.example/" :headers {"Authorization" (secret/hide "Bearer s3cr3t\n")}})
             (fn [{:keys [error]}]
               (is (= {:type ::http/invalid-header :url "https://a.example/" :header "Authorization"} (ex-data error)))
               (is (not (re-find #"s3cr3t" (ex-message error))))
               (is (empty? @asked) "the plugin gets no request, since a native client can throw with the value"))))))
