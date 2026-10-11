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

(defn- pour-body
  "A promise of how many bytes of the body of the fetch response `res` from
  `url` went to `sink`, a function of each piece that can give a promise to
  wait for: the first `n` of them, the rest left unread, or with nil `n`
  the whole body, rejected with ::too-large once more than `max-bytes`
  arrive."
  [^js res url n max-bytes sink]
  (let [reader (.getReader (.-body res))
        cancel #(.catch (.cancel reader) (fn [_] nil))]
    (letfn [(step [got]
              (.then (.read reader)
                     (fn [^js r]
                       (if (.-done r)
                         got
                         (let [chunk (.-value r)
                               room  (if n (- n got) ##Inf)
                               piece (if (< room (.-length chunk))
                                       (.subarray chunk 0 room)
                                       chunk)
                               sum   (+ got (.-length piece))]
                           (if (and (nil? n) max-bytes (> sum max-bytes))
                             (do (cancel)
                                 (throw (limits/too-large url max-bytes)))
                             (.then (js/Promise.resolve (sink piece))
                                    (fn [_]
                                      (if (and n (>= sum n))
                                        (do (cancel) sum)
                                        (step sum))))))))))]
      (step 0))))

(defn- answer
  "A promise of the response map for the fetch response `res` to the
  `request`. The body of a success goes to the :sink of the request, when
  it has one."
  [^js res {:keys [method url as range max-bytes sink truncate?] :as request}]
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
                                       max-bytes)
        cut?     (and truncate? max-bytes (nil? n))
        ;; one byte more, to tell a body cut short from one that just fits
        how      (cond
                   n    {:first n}
                   cut? {:first (inc max-bytes)}
                   :else {:max-bytes max-bytes})]
    (cond
      (or (= :head method) (nil? (.-body res)))
      response

      (and sink (<= 200 status 299))
      (.then (pour-body res url n max-bytes sink)
             #(assoc response :received %))

      :else
      (.then (read-body res url how)
             (fn [b]
               (let [over? (and cut? (> (.-length b) max-bytes))
                     b     (if over? (.subarray b 0 max-bytes) b)]
                 (cond-> response
                   (pos? (.-length b))
                   (assoc :body
                          (if (= :bytes as)
                            b
                            (encoding/decode b (headers/charset headers))))

                   over?
                   (assoc :truncated? true))))))))

(defn- offline?
  "Whether the fetch failed with `e` since the device is offline. Fetch
  says nothing more of why a request failed, but the browser knows when
  it has no network."
  [e]
  (and (instance? js/TypeError e)
       (exists? js/navigator)
       (false? (.-onLine js/navigator))))

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
          (.catch #(throw (if (offline? %) (limits/unreachable url %) %)))
          (.then #(answer % request))))))
