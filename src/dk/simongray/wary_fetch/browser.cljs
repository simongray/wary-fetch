(ns ^:no-doc dk.simongray.wary-fetch.browser
  "The client of a browser, and of a runtime without the modules of Node:
  fetch, which follows redirects itself, and which lets a page read an
  answer from another site only as far as the CORS headers of the answer
  allow."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.encoding :as encoding]
            [dk.simongray.wary-fetch.headers :as headers]
            [dk.simongray.wary-fetch.limits :as limits]))

(def available?
  "Whether the code runs in a page or a worker of a browser. There, a
  header that a request sets can make fetch send a preflight request
  first, and the browser sends its own User-Agent."
  (or (exists? js/document) (exists? js/WorkerGlobalScope)))

(defn- read-body
  "A promise of the body of the fetch response `res` from `url` as bytes,
  as much as `opts` say:

  - With :first, it's that many bytes from the start, and the rest is left
    unread.
  - With :max-bytes, it's the whole body, and the promise is rejected with
    ::too-large once more bytes arrive.
  - Without either, it's the whole body."
  [^js res url {:keys [max-bytes] :as opts}]
  (let [reader (.getReader (.-body res))
        n      (or (:first opts) max-bytes ##Inf)
        all?   (nil? (:first opts))]
    (letfn [(join [chunks]
              (.catch (.cancel reader) (fn [_] nil))
              (let [size (min n (reduce + (map #(.-length %) chunks)))
                    out  (js/Uint8Array. size)]
                (reduce (fn [p chunk]
                          (let [k (min (.-length chunk) (- size p))]
                            (.set out (.subarray chunk 0 k) p)
                            (+ p k)))
                        0 chunks)
                out))
            (step [chunks got]
              (cond
                (and all? (> got n))
                (do (.catch (.cancel reader) (fn [_] nil))
                    (throw (limits/too-large url n)))

                (and (not all?) (>= got n))
                (join chunks)

                :else
                (.then (.read reader)
                       (fn [r]
                         (if (.-done r)
                           (join chunks)
                           (let [chunk (.-value r)]
                             (step (conj chunks chunk)
                                   (+ got (.-length chunk)))))))))]
      (js/Promise.resolve (step [] 0)))))

(defn- answer
  "A promise of the response map for the fetch response `res` to the
  `request`."
  [^js res {:keys [method url as range max-bytes] :as request}]
  (let [status   (.-status res)
        received (headers/content-range (.get (.-headers res) "content-range"))
        headers  (into {}
                       (map (fn [[k v]] [k [v]]))
                       (es6-iterator-seq (.entries (.-headers res))))
        response (cond-> {:status  status
                          :url     (.-url res)
                          :headers headers}
                   received                (assoc :content-range received)
                   (= "cors" (.-type res)) (assoc :filtered? true))
        ;; fetch undoes gzip itself, so a prefix is one of the file
        n        (limits/prefix-length status (:start received) false range
                                       max-bytes)]
    (if (or (= :head method) (nil? (.-body res)))
      response
      (.then (read-body res url (if n {:first n} {:max-bytes max-bytes}))
             (fn [b]
               (cond-> response
                 (pos? (.-length b))
                 (assoc :body
                        (if (= :bytes as)
                          b
                          (encoding/decode b (headers/charset headers))))))))))

(defn send!
  "A promise of the response to `request`, sent with fetch within
  `timeout-seconds`, unless `allow-pred` refuses its URL."
  [{:keys [method url headers body max-redirects] :as request}
   timeout-seconds
   allow-pred]
  (if-not (allow-pred url)
    (js/Promise.reject (limits/refusal url))
    (let [signal (js/AbortSignal.timeout (* 1000 timeout-seconds))
          init   (cond-> {:method  (str/upper-case (name (or method :get)))
                          :headers headers
                          :signal  signal}
                   (some? body)        (assoc :body body)
                   (= 0 max-redirects) (assoc :redirect "manual"))]
      (-> (js/fetch url (clj->js init))
          (.then #(answer % request))))))
