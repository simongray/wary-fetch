(ns dk.simongray.wary-fetch.capacitor
  "A :send over CapacitorHttp, the plugin that sends the requests of a
  Capacitor app from native code, where CORS doesn't stop them.

      (ns my.app
        (:require [\"@capacitor/core\" :refer [CapacitorHttp]]
                  [dk.simongray.wary-fetch.capacitor :as capacitor]))

      (def send (capacitor/sender CapacitorHttp))

  It sends a request as send! does, with the same limits, and gives the
  same response. It follows the redirects itself, so that :allow-pred
  takes each URL, and it decodes text by its charset, since the plugin
  reads all text as UTF-8. The plugin hands a body over whole, so the
  sender suits feeds and the calls of APIs rather than episodes, which
  the plugin's docs leave to a plugin for file transfers. Capacitor's HTTP
  API is at https://capacitorjs.com/docs/apis/http"
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.encoding :as encoding]
            [dk.simongray.wary-fetch.headers :as headers]
            [dk.simongray.wary-fetch.json :as json]
            [dk.simongray.wary-fetch.limits :as limits]
            [dk.simongray.wary-fetch.redirect :as redirect]
            [dk.simongray.wary-fetch.url :as url]
            [dk.simongray.webcrypto.bytes :as bytes]))

(defn- unbase64
  "The bytes of the base64 text `s`."
  [s]
  (js/Uint8Array.from (js/atob s) #(.charCodeAt % 0)))

(defn- options
  "The options of CapacitorHttp.request for the prepared `request`, within
  `timeout-ms`. The body comes as bytes in base64, and the plugin follows
  no redirect."
  [{:keys [method url headers body range user-agent]} timeout-ms]
  (let [agent   (or user-agent http/user-agent)
        headers (cond-> (merge {"User-Agent" agent} headers)
                  ;; the offsets of a range are those of the file
                  range (assoc "Accept-Encoding" "identity"))]
    (clj->js
     (cond-> {:url                   url
              :method                (str/upper-case (name (or method :get)))
              :headers               headers
              :responseType          "arraybuffer"
              :disableRedirects      true
              :shouldEncodeUrlParams false
              :connectTimeout        timeout-ms
              :readTimeout           timeout-ms}
       (string? body)                 (assoc :data body)
       (instance? js/Uint8Array body) (assoc :data     (bytes/base64 body)
                                             :dataType "file")))))

(defn- headers-of
  "The headers of the answer `res` of the plugin, by their names in lower
  case, each with its value in a vector, as send! gives them."
  [^js res]
  (into {}
        (map (fn [[k v]] [(str/lower-case k) [(str v)]]))
        (js->clj (or (.-headers res) #js {}))))

(defn- body-of
  "The bytes of the `data` that the plugin gave for an answer with the
  `headers`. It gives bytes in base64, but the value of a JSON body, which
  it parses whatever was asked for, and its text when that doesn't parse."
  [data headers]
  (let [json? (str/includes? (str (headers/header headers "content-type"))
                             "application/json")]
    (cond
      (nil? data)                     (bytes/of [])
      (instance? js/ArrayBuffer data) (js/Uint8Array. data)
      (and (string? data) json?)      (bytes/encode data)
      (string? data)                  (unbase64 data)
      :else                           (bytes/encode (js/JSON.stringify data)))))

(defn- within
  "The promise `p`, or one that rejects with the exception of `late` at the
  instant `deadline`, whichever settles first."
  [p deadline late]
  (let [timer (volatile! nil)
        alarm (js/Promise.
               (fn [_ reject]
                 (vreset! timer (js/setTimeout #(reject (late))
                                               (max 0 (- deadline
                                                         (js/Date.now)))))))]
    (.finally (js/Promise.race #js [p alarm])
              #(js/clearTimeout @timer))))

(defn- send-following!
  "A promise of the answer of the plugin `http` to `request`, its headers,
  the redirects before it and the last request, as a vector. It follows
  the redirects until the instant `deadline` and checks each URL with
  `allow-pred`. A header value that the JDK and node:http refuse rejects
  with ::invalid-header, since a native client can throw with the value."
  [^js http request deadline allow-pred]
  (letfn [(step [{:keys [url headers body] :as request} chain]
            (cond
              (not (allow-pred url))
              (js/Promise.reject (limits/refusal url))

              (limits/unsendable-header headers)
              (js/Promise.reject (limits/invalid-header url headers))

              (coll? body)
              (js/Promise.reject (limits/invalid-body url))

              :else
              (let [left (- deadline (js/Date.now))]
                (-> (within (.request http (options request left))
                            deadline
                            #(limits/late url))
                    (.catch (fn [e]
                              (throw (if (ex-data e)
                                       e
                                       (limits/unreachable url e)))))
                    (.then (fn [^js res]
                             (let [status  (.-status res)
                                   headers (headers-of res)
                                   limit   (:max-redirects request
                                                           redirect/limit)
                                   target  (when (and (redirect/statuses status)
                                                      (< (count chain) limit))
                                             (redirect/target
                                              url
                                              (headers/header headers
                                                              "location")))]
                               (if target
                                 (step (redirect/next-request request status
                                                              target)
                                       (conj chain [status url headers]))
                                 [res headers chain request]))))))))]
    (step (update request :url url/legal-url) [])))

(defn- answer
  "A promise of the response map for the answer `res` of the plugin, with
  `headers`, to the `request` after the redirects `chain`. The body of a
  success goes to the :sink of the request, when it has one."
  [^js res headers chain
   {:keys [method url as range max-bytes truncate? sink] :as request}]
  (let [status   (.-status res)
        received (headers/content-range (headers/header headers
                                                        "content-range"))
        whole    (if (= :head method)
                   (bytes/of [])
                   (body-of (.-data res) headers))
        n        (limits/prefix-length status (:start received) false range
                                       max-bytes)
        b        (cond-> whole
                   (and n (< n (bytes/length whole))) (.subarray 0 n))
        over?    (and (nil? n) max-bytes (> (bytes/length b) max-bytes))
        b        (cond-> b
                   (and over? truncate?) (.subarray 0 max-bytes))
        moved    (redirect/permanent-url chain url)
        response (cond-> {:status status :url url :headers headers}
                   received              (assoc :content-range received)
                   moved                 (assoc :permanent-url moved)
                   (seq chain)           (assoc :redirects chain)
                   (and over? truncate?) (assoc :truncated? true))]
    (cond
      (and over? (not truncate?))
      (js/Promise.reject (limits/too-large url max-bytes))

      (and sink (not= :head method) (<= 200 status 299))
      (.then (js/Promise.resolve (sink b))
             (fn [_] (assoc response :received (bytes/length b))))

      (or (zero? (bytes/length b)) (= 304 status))
      (js/Promise.resolve response)

      :else
      (js/Promise.resolve
       (assoc response :body (if (= :bytes as)
                               b
                               (encoding/decode b headers)))))))

(defn sender
  "A function to give as :send, which sends each request with `http`, the
  CapacitorHttp plugin of @capacitor/core, and gives a promise of the
  response, as send! does.

  The keys of a request are those of send!, and a :sink is a function of
  the bytes, which it gets in one piece. A failure that the plugin reports
  is ::http/unreachable, since it says no more of why. The plugin can't
  stop a request, so the :signal of one is checked before it's sent."
  [^js http]
  (fn [{:keys [url timeout-seconds allow-pred as signal]
        :or   {timeout-seconds 30 allow-pred (constantly true)}
        :as   request}]
    (if (limits/stopped? signal)
      (js/Promise.reject (limits/aborted url))
      (let [request  (http/prepared request)
            deadline (+ (js/Date.now) (* 1000 timeout-seconds))
            sent     (.then (send-following! http request deadline
                                             allow-pred)
                            (fn [[res headers chain last-request]]
                              (answer res headers chain last-request)))]
        (if (= :json as)
          (.then sent #(update % :body json/read-json))
          sent)))))
