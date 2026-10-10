(ns ^:no-doc dk.simongray.wary-fetch.node
  "The client of send! in Node: node:dns looks up the names, and node:http
  and node:https connect only to the addresses that were looked up.

  The modules load when they're first used, so a page that holds this
  code loads none of them."
  (:require [clojure.string :as str]))

(def available?
  "Whether the code runs in Node, or in a runtime that has its modules."
  (boolean (and (exists? js/process)
                (some-> js/process .-versions .-node)
                (or (exists? js/process.getBuiltinModule)
                    (exists? js/require)))))

(defn- module
  "The built-in module `module-name` of Node, e.g. node:http."
  [module-name]
  (if (exists? js/process.getBuiltinModule)
    (.getBuiltinModule js/process module-name)
    (js/require module-name)))

(defn- by-deadline
  "The promise `p`, or one that rejects with what `late` gives at the
  instant `deadline` in epoch milliseconds, whichever settles first."
  [p deadline late]
  (let [timer (volatile! nil)]
    (-> (js/Promise.race
         #js [p (js/Promise. (fn [_ reject]
                               (vreset! timer (js/setTimeout
                                               #(reject (late))
                                               (max 0 (- deadline
                                                         (js/Date.now)))))))])
        (.finally #(js/clearTimeout @timer)))))

(defn lookup!
  "A promise of the addresses that the domain name `host` resolves to,
  before the instant `deadline` or else rejected with what `late` gives.
  Each address is a map of its text as :address and its :family, 4 or 6."
  [host deadline late]
  (-> (.lookup (.-promises (module "node:dns")) host #js {:all true})
      (.then (fn [found]
               (mapv (fn [^js a] {:address (.-address a) :family (.-family a)})
                     found)))
      (by-deadline deadline late)))

(defn- headers-of
  "The headers of the answer `res` of Node, as a map from lower-case names
  to vectors of values, in the order of the answer."
  [^js res]
  (reduce (fn [m [k v]] (update m (str/lower-case k) (fnil conj []) v))
          {}
          (partition 2 (.-rawHeaders res))))

(defn- options
  "The options of node:http for `request`, a map of :method, :url,
  :headers and :body as texts, that connect only to the `addresses`, or to
  the address in the URL when they're nil."
  [{:keys [method url headers body]} addresses]
  (let [^js u  (js/URL. url)
        port   (.-port u)
        pinned (fn [_ ^js opts callback]
                 (if (.-all opts)
                   (callback nil (clj->js addresses))
                   (let [{:keys [address family]} (first addresses)]
                     (callback nil address family))))
        length (when (and (some? body)
                          (not-any? #(= "content-length" (str/lower-case %))
                                    (keys headers)))
                 (.byteLength js/Buffer body))]
    (cond-> {:method   (str/upper-case (name (or method :get)))
             :hostname (str/replace (.-hostname u) #"^\[|\]$" "")
             :path     (str (.-pathname u) (.-search u))
             :headers  (cond-> headers
                         length (assoc "Content-Length" (str length)))
             ;; a socket of a shared pool can lead to an address that
             ;; nobody checked for this request
             :agent    false}
      (seq port)      (assoc :port (js/parseInt port 10))
      (seq addresses) (assoc :lookup pinned))))

(defn open!
  "A promise of the answer to `request`, before its body is read: a map
  of its :status and :headers, and what close! and read! take. The
  connection goes only to the `addresses`, unless they're nil.

  The exchange, the body of the answer included, ends at the instant
  `deadline` in epoch milliseconds, with the exception that `late` gives."
  [request addresses deadline late]
  (js/Promise.
   (fn [resolve reject]
     (let [^js client (module (if (str/starts-with? (:url request) "https:")
                                "node:https"
                                "node:http"))
           ^js req    (.request client (clj->js (options request addresses)))
           res    (volatile! nil)
           fate   (volatile! nil)
           timer  (js/setTimeout (fn []
                                   (let [e (late)]
                                     (vreset! fate e)
                                     (.destroy req e)
                                     (some-> @res (.destroy e))))
                                 (max 0 (- deadline (js/Date.now))))]
       (.on req "error" (fn [e]
                          (js/clearTimeout timer)
                          (reject (or @fate e))))
       (.on req "response" (fn [^js r]
                             (vreset! res r)
                             (resolve {:status   (.-statusCode r)
                                       :headers  (headers-of r)
                                       :response r
                                       :timer    timer
                                       :fate     fate})))
       (when-some [body (:body request)]
         (.write req body))
       (.end req)))))

(defn close!
  "Close the `answer` that open! gave, without reading the rest of it."
  [{:keys [^js response timer]}]
  (js/clearTimeout timer)
  (.destroy response))

(defn read!
  "A promise of the bytes of the body of the `answer` that open! gave:
  the first `n` of them, the rest left unread, or with nil `n` the whole
  body. Once more than `max-bytes` arrive, it rejects with what
  `too-large` gives."
  [{:keys [^js response timer fate]} n max-bytes too-large]
  (js/Promise.
   (fn [resolve reject]
     (let [chunks (array)
           total  (volatile! 0)
           done   (volatile! false)
           finish (fn [f x]
                    (when-not @done
                      (vreset! done true)
                      (js/clearTimeout timer)
                      (f x)))
           joined (fn [k]
                    (let [b (.concat js/Buffer chunks)]
                      (js/Uint8Array. (.subarray b 0 (min k (.-length b))))))]
       (.on response "data"
            (fn [chunk]
              (.push chunks chunk)
              (vswap! total + (.-length chunk))
              (cond
                (and n (>= @total n))
                (do (finish resolve (joined n))
                    (.destroy response))

                (and (nil? n) max-bytes (> @total max-bytes))
                (do (finish reject (too-large))
                    (.destroy response)))))
       (.on response "end" #(finish resolve (joined @total)))
       (.on response "error" #(finish reject (or @fate %)))
       (.on response "close"
            #(finish reject (or @fate
                                (js/Error. "The answer ended early"))))))))

(defn gunzip
  "The gzipped bytes `b` inflated, or nil when they'd be more than
  `max-bytes`, which is no limit when it's nil."
  [b max-bytes]
  (try
    (js/Uint8Array. (.gunzipSync (module "node:zlib") b
                                 (if max-bytes
                                   #js {:maxOutputLength (max 1 max-bytes)}
                                   #js {})))
    (catch :default e
      (if (= "ERR_BUFFER_TOO_LARGE" (.-code ^js e))
        nil
        (throw e)))))
