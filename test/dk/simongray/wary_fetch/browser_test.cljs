(ns dk.simongray.wary-fetch.browser-test
  "send! in a browser, over fetch, with the files of the page's own origin,
  as test/browser-run.mjs or any static server serves them: bodies,
  ranges, limits, the sink, and the failures that fetch doesn't type."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.test-support :refer [run utf8]]))

(def page
  "The URL of the page of the tests, which the server of the build gives."
  (str js/location.origin "/index.html"))

(defn- failure-type
  "A promise of the :type of the exception of the attempt to send
  `request`."
  [request]
  (.then (http/try-send! http/send! request)
         (fn [{:keys [error]}]
           (:type (ex-data error)))))

(defn- slow-sink
  "A sink that keeps the pieces in the atom `pieces` and takes `ms`
  milliseconds over each."
  [pieces ms]
  (fn [b]
    (swap! pieces into (vec b))
    (js/Promise. (fn [resolve] (js/setTimeout resolve ms)))))

(deftest a-file-of-the-page
  (let [text (atom nil)]
    (run
      #(.then (http/send! {:url page})
              (fn [{:keys [status url body] :as response}]
                (reset! text body)
                (is (= [200 page] [status url]))
                (is (str/includes? body "<script") "the body as text")
                (is (not (:filtered? response))
                    "a response of the page's own origin, which CORS shows")))
      #(.then (http/send! {:url page :as :bytes})
              (fn [{:keys [body]}]
                (is (instance? js/Uint8Array body))
                (is (= @text (utf8 body)) "the body as bytes")))
      #(.then (http/send! {:url page :range [0 9]})
              (fn [{:keys [body]}]
                (is (= (subs @text 0 10) body)
                    "the first bytes, whether the server sends a range or all")))
      #(.then (http/send! {:url page :method :head})
              (fn [{:keys [status] :as response}]
                (is (= 200 status))
                (is (not (contains? response :body)) "no body for HEAD")))
      #(.then (http/send! {:url (str js/location.origin "/no-such-file")})
              (fn [{:keys [status]}]
                (is (= 404 status) "an error status is a response"))))))

(deftest the-limit-of-a-body
  (run
    #(.then (failure-type {:url page :max-bytes 10})
            (fn [type]
              (is (= ::http/too-large type))))
    #(.then (http/send! {:url page :max-bytes 10 :truncate? true})
            (fn [{:keys [body truncated?]}]
              (is (= 10 (count body)))
              (is truncated? "a body cut at the limit")))
    #(.then (http/send! {:url page :max-bytes 1000000 :truncate? true})
            (fn [response]
              (is (not (:truncated? response)) "a body within it is whole")))))

(deftest a-body-in-pieces
  (let [whole  (atom nil)
        pieces (atom [])]
    (run
      #(.then (http/send! {:url page :as :bytes})
              (fn [{:keys [body]}]
                (reset! whole (vec body))))
      #(.then (http/send! {:url page :sink (slow-sink pieces 0)})
              (fn [{:keys [received] :as response}]
                (is (= @whole @pieces) "every piece in order")
                (is (= (count @whole) received))
                (is (not (contains? response :body)))))
      #(do (reset! pieces [])
           (.then (http/send! {:url page :range [0 9] :sink (slow-sink pieces 0)})
                  (fn [{:keys [received]}]
                    (is (= [10 (subvec @whole 0 10)] [received @pieces])
                        "the first bytes of a range, and the rest unread"))))
      #(.then (failure-type {:url page :max-bytes 10 :sink (slow-sink pieces 0)})
              (fn [type]
                (is (= ::http/too-large type) "the limit of a sink"))))))

(deftest failures-of-a-browser
  (let [stopped  (js/AbortController.)
        stopping (js/AbortController.)]
    (.abort stopped)
    (run
      #(.then (failure-type {:url page :allow-pred (constantly false)})
              (fn [type]
                (is (= ::http/refused type))))
      #(.then (failure-type {:url page :headers {"X-Token" "t\n"}})
              (fn [type]
                (is (= ::http/invalid-header type)
                    "a value that the JDK and node:http refuse, though fetch takes it")))
      #(.then (failure-type {:url page :signal (.-signal stopped)})
              (fn [type]
                (is (= ::http/aborted type) "a signal that stopped before")))
      #(let [sent (failure-type {:url page :signal (.-signal stopping)})]
         (.abort stopping)
         (.then sent
                (fn [type]
                  (is (= ::http/aborted type) "and one that stops fetch")))))))
