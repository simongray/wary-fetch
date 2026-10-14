(ns ^:no-doc dk.simongray.wary-fetch.browser
  "The client over fetch, in a browser or in a runtime without Node's
  modules, e.g. React Native.

  In a browser, fetch follows redirects itself and lets a page read an
  answer from another origin only as far as the answer's CORS headers
  allow. Elsewhere nothing limits what fetch reaches, so the client
  follows the redirects and checks each URL, as in Node."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.encoding :as encoding]
            [dk.simongray.wary-fetch.headers :as headers]
            [dk.simongray.wary-fetch.limits :as limits]
            [dk.simongray.wary-fetch.redirect :as redirect]
            [dk.simongray.wary-fetch.url :as url]))

(def available?
  "Whether the code runs in a page or a worker of a browser. There, a
  header that a request sets can make fetch send a preflight request
  first, and the browser sends its own User-Agent."
  (or (exists? js/document) (exists? js/WorkerGlobalScope)))

(defn- reader-of
  "A reader of the body of the fetch response `res`: its stream's, or one
  that reads the whole body as a single chunk, for a fetch whose responses
  have no stream, e.g. React Native's own."
  [^js res]
  (if-let [body (.-body res)]
    (.getReader body)
    (let [read? (volatile! false)]
      #js {:read   (fn []
                     (if @read?
                       (js/Promise.resolve #js {:done true})
                       (do (vreset! read? true)
                           (.then (.arrayBuffer res)
                                  #(js-obj "done" false
                                           "value" (js/Uint8Array. %))))))
           :cancel (fn [] (js/Promise.resolve nil))})))

(defn- read-body
  "A promise of the body of the fetch response `res` from `url` as bytes,
  as much of it as `opts` ask for:

  - With :first, it's that many bytes from the start, and the rest stays
    unread.
  - With :max-bytes, it's the whole body, and the promise rejects with
    ::too-large once more bytes arrive.
  - Without either, it's the whole body."
  [^js res url {:keys [max-bytes] :as opts}]
  (let [reader (reader-of res)
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
  `url` went to `sink`, a function of each piece that can return a promise
  to wait for. The sink gets the first `n` bytes and the rest stays
  unread, or with nil `n` it gets the whole body. The promise rejects with
  ::too-large once more than `max-bytes` arrive."
  [^js res url n max-bytes sink]
  (let [reader (reader-of res)
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

(defn- headers-of
  "The headers of the fetch response `res`, by their names in lower case,
  each with its value in a vector, as send! returns them."
  [^js res]
  (into {}
        (map (fn [[k v]] [k [v]]))
        (es6-iterator-seq (.entries (.-headers res)))))

(defn- answer
  "A promise of the response map for the fetch response `res` to the
  `request`, after the redirects `chain`. The body of a success goes to
  the :sink of the request, when it has one."
  [^js res chain
   {:keys [method url as range max-bytes sink truncate?] :as request}]
  (let [status   (.-status res)
        received (headers/content-range (.get (.-headers res) "content-range"))
        headers  (headers-of res)
        moved    (redirect/permanent-url chain url)
        response (cond-> {:status  status
                          :url     (if (str/blank? (.-url res)) url (.-url res))
                          :headers headers}
                   received                (assoc :content-range received)
                   moved                   (assoc :permanent-url moved)
                   (seq chain)             (assoc :redirects chain)
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
      ;; Fetch, section 2.2.3: a null body status. Any other body is read,
      ;; even without a stream, which React Native's own fetch never gives
      (or (= :head method) (#{101 103 204 205 304} status))
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
                            (encoding/decode b headers)))

                   over?
                   (assoc :truncated? true))))))))

(defn- offline?
  "Whether the fetch failed with `e` since the device is offline. Fetch
  doesn't report why a request failed, but the browser reports when it
  has no network."
  [e]
  (and (instance? js/TypeError e)
       (exists? js/navigator)
       (false? (.-onLine js/navigator))))

;; DOM Standard, section 3.1: AbortSignal.any
(defn- both
  "An AbortSignal that aborts when `timeout` or the AbortSignal `stop`
  does, if there is one. It makes the signal with AbortSignal.any, or by
  hand in a browser without it."
  [^js timeout ^js stop]
  (cond
    (nil? stop)                timeout
    (exists? js/AbortSignal.any) (js/AbortSignal.any #js [timeout stop])
    :else                      (let [c     (js/AbortController.)
                                     abort #(.abort c (.-reason ^js %))]
                                 (.addEventListener timeout "abort"
                                                    #(abort timeout))
                                 (.addEventListener stop "abort"
                                                    #(abort stop))
                                 (.-signal c))))

(defn- settled-by
  "The promise `p`, or one that rejects with the reason of the AbortSignal
  `signal` once it aborts, whichever settles first. The fetch of Expo
  doesn't reject when its signal aborts before the answer comes."
  [p ^js signal]
  (let [listener (volatile! nil)
        aborted  (js/Promise.
                  (fn [_ reject]
                    (if (.-aborted signal)
                      (reject (.-reason signal))
                      (do (vreset! listener #(reject (.-reason signal)))
                          (.addEventListener signal "abort" @listener)))))]
    (.finally (js/Promise.race #js [p aborted])
              #(some->> @listener (.removeEventListener signal "abort")))))

(defn- fetch-hop!
  "A promise of the fetch response to the `request` of a single hop, before
  its body is read, unless `allow-pred` refuses its URL. The AbortSignal
  `signal` stops it. With `manual?`, fetch returns a redirect rather than
  follow it."
  [request ^js signal allow-pred manual?]
  ;; so that the runtime's parser, which parses the URL again, gets the
  ;; host that was checked
  (let [{:keys [method url headers body max-redirects] stop :signal}
        (update request :url url/legal-url)]
    (cond
      (not (allow-pred url))
      (js/Promise.reject (limits/refusal url))

      (limits/unsendable-header headers)
      (js/Promise.reject (limits/invalid-header url headers))

      (coll? body)
      (js/Promise.reject (limits/invalid-body url))

      (limits/stopped? stop)
      (js/Promise.reject (limits/aborted url))

      :else
      ;; fetch refuses null headers, which a request without any has
      (let [manual? (or manual? (= 0 max-redirects))]
        (js/fetch url (clj->js
                       (cond-> {:method (str/upper-case (name (or method :get)))
                                :signal signal}
                         (seq headers) (assoc :headers headers)
                         (some? body)  (assoc :body body)
                         manual?       (assoc :redirect "manual"))))))))

(defn- following!
  "A promise of the fetch response to `request`, the redirects before it
  and the last request, as a vector. It follows the redirects itself and
  checks each URL with `allow-pred`, for a runtime where no same-origin
  policy limits what fetch reaches, e.g. React Native. The AbortSignal
  `signal` stops it. The :prepare-fn of the request, when it has one,
  returns the request of each hop."
  [request ^js signal allow-pred]
  (letfn [(step [request chain]
            (.then (redirect/hop request)
                   #(send-hop (update % :url url/legal-url) chain)))
          (send-hop [{:keys [url] :as request} chain]
            (.then (fetch-hop! request signal allow-pred true)
                   (fn [^js res]
                     (let [status (.-status res)
                           limit  (:max-redirects request redirect/limit)
                           target (when (and (redirect/statuses status)
                                             (< (count chain) limit))
                                    (redirect/target
                                     url (.get (.-headers res) "location")))
                           hop    [status url (headers-of res)]]
                       (if-not target
                         [res chain request]
                         (do (some-> (.-body res) .cancel (.catch (fn [_] nil)))
                             (step (redirect/next-request request status target)
                                   (conj chain hop))))))))]
    (step request [])))

(defn send!
  "A promise of the response to `request`, sent with fetch within
  `timeout-seconds`, unless `allow-pred` refuses its URL. It rejects with
  ::aborted when the AbortSignal of its :signal aborts.

  In a browser, fetch follows the redirects, and the browser's same-origin
  policy limits what a page reads. So the :prepare-fn of the request
  returns the request of the first hop alone. Elsewhere, e.g. in React
  Native, the client follows them itself and checks each URL with
  `allow-pred`, as the Node client does.

  A header value that the JDK and node:http refuse (e.g. one with a line
  break) rejects with ::invalid-header before fetch gets it, since Node's
  fetch throws with the value."
  [{stop :signal :as request} timeout-seconds allow-pred]
  (let [url    (url/legal-url (:url request))
        late   (js/AbortSignal.timeout (* 1000 timeout-seconds))
        signal (both late stop)
        failed (fn [e]
                 (cond
                   (limits/stopped? stop) (limits/aborted url)
                   (.-aborted late)       (limits/late url)
                   (offline? e)           (limits/unreachable url e)
                   :else                  e))]
    (-> (if available?
          (.then (redirect/hop request)
                 (fn [request]
                   (.then (fetch-hop! request signal allow-pred false)
                          #(vector % [] request))))
          (following! request signal allow-pred))
        (.then (fn [[res chain last-request]] (answer res chain last-request)))
        (settled-by signal)
        (.catch #(throw (failed %))))))
