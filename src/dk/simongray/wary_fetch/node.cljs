(ns ^:no-doc dk.simongray.wary-fetch.node
  "The client in Node: node:dns looks up the names, and node:http and
  node:https connect only to the addresses that it returned. This
  namespace follows redirects itself and reads bodies within the limits of
  time and size.

  The modules load when they're first used, so a page that holds this
  code loads none of them."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.encoding :as encoding]
            [dk.simongray.wary-fetch.headers :as headers]
            [dk.simongray.wary-fetch.limits :as limits]
            [dk.simongray.wary-fetch.redirect :as redirect]
            [dk.simongray.wary-fetch.url :as url]))

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
  "The promise `p`, or one that rejects with what `late` returns at the
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
  before the instant `deadline` or else rejected with what `late` returns.
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

  The exchange, the answer's body included, ends at the instant `deadline`
  in epoch milliseconds with the exception that `late` returns, or with
  ::aborted when the request's :signal aborts."
  [{:keys [url signal] :as request} addresses deadline late]
  (js/Promise.
   (fn [resolve reject]
     (let [^js client (module (if (str/starts-with? url "https:")
                                "node:https"
                                "node:http"))
           ^js req    (.request client (clj->js (options request addresses)))
           res    (volatile! nil)
           fate   (volatile! nil)
           end!   (fn [e]
                    (vreset! fate e)
                    (.destroy req e)
                    (some-> @res (.destroy e)))
           timer  (js/setTimeout #(end! (late))
                                 (max 0 (- deadline (js/Date.now))))]
       (when signal
         (.addEventListener ^js signal "abort"
                            #(end! (limits/aborted url))
                            #js {:once true}))
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
  "Close the `answer` that open! returned, without reading the rest of it."
  [{:keys [^js response timer]}]
  (js/clearTimeout timer)
  (.destroy response))

(defn read!
  "A promise of the bytes of the body of the `answer` that open! returned:
  the first `n` of them, the rest left unread, or with nil `n` the whole
  body. Once more than `max-bytes` arrive, it rejects with what
  `too-large` returns."
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

(defn- given!
  "Pass the `piece` of a body to `sink` (a function of each piece or a
  writable stream), and return what to wait for before the next piece:
  the promise that the function returns, or the drain of a full stream."
  [sink piece]
  (if (fn? sink)
    (sink piece)
    (when-not (.write ^js sink piece)
      (js/Promise. (fn [resolve] (.once ^js sink "drain" resolve))))))

(defn pour!
  "A promise of how many bytes of the body of the `answer` that open!
  returned went to `sink` through given!, with gzip undone when it's
  `gzipped?`: the first `n` of them, the rest left unread, or with nil `n`
  the whole body. Once more than `max-bytes` arrive, it rejects with what
  `too-large` returns."
  [{:keys [^js response timer fate]} n max-bytes gzipped? sink too-large]
  (js/Promise.
   (fn [resolve reject]
     (let [^js source (if gzipped?
                        (.pipe response (.createGunzip (module "node:zlib")))
                        response)
           total      (volatile! 0)
           done       (volatile! false)
           finish     (fn [f x]
                        (when-not @done
                          (vreset! done true)
                          (js/clearTimeout timer)
                          (f x)))
           stop       (fn [f x]
                        (finish f x)
                        (.destroy response))]
       (.on source "data"
            (fn [^js chunk]
              (.pause source)
              (let [room  (if n (- n @total) ##Inf)
                    piece (if (< room (.-length chunk))
                            (.subarray chunk 0 room)
                            chunk)
                    sum   (vswap! total + (.-length piece))]
                (if (and (nil? n) max-bytes (> sum max-bytes))
                  (stop reject (too-large))
                  (.then (js/Promise.resolve (given! sink piece))
                         (fn [_]
                           (if (and n (>= sum n))
                             (stop resolve sum)
                             (.resume source)))
                         #(stop reject %))))))
       (.on source "end" #(finish resolve @total))
       (.on source "error" #(finish reject (or @fate %)))
       (.on response "error" #(finish reject (or @fate %)))
       ;; the gunzip ends after the answer closes
       (when-not gzipped?
         (.on response "close"
              #(finish reject (or @fate
                                  (js/Error. "The answer ended early")))))))))

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

(defn- checked!
  "A promise of the addresses that the request to `url` may connect to
  once `allow-pred` accepts the URL, by the instant `deadline`, or of nil
  for a host that's an address. It rejects with ::refused when
  `allow-pred` refuses the URL.

  It looks up a name first, so that url/public? checks its addresses
  too."
  [url allow-pred deadline]
  (let [host (some-> (url/host url) (str/replace #"\.$" ""))
        ask  (fn [found]
               (let [numbers (mapv (comp url/ip-address :address) found)]
                 (if (binding [url/*addresses* (cond-> {}
                                                 found (assoc host numbers))]
                       (allow-pred url))
                   found
                   (throw (limits/refusal url)))))]
    (if (or (nil? host) (url/ip-address host))
      (.then (js/Promise.resolve nil) ask)
      (.then (lookup! host deadline #(limits/late url)) ask))))

(defn- connect-failure
  "The exception of the request to `url` that failed with `e` before an
  answer: typed when the code of `e` says that the name didn't resolve,
  that the host couldn't be reached or that the secure connection failed,
  and else `e`."
  [url ^js e]
  (let [code (str (some-> e .-code))]
    (cond
      (contains? #{"ENOTFOUND" "EAI_AGAIN" "EAI_NONAME" "EAI_NODATA"
                   "EAI_FAIL"}
                 code)
      (limits/unresolved url e)

      (contains? #{"ECONNREFUSED" "EHOSTUNREACH" "ENETUNREACH" "ENETDOWN"
                   "EHOSTDOWN" "EADDRNOTAVAIL" "ETIMEDOUT"}
                 code)
      (limits/unreachable url e)

      (or (re-find #"^(ERR_TLS_|ERR_SSL_|CERT_|UNABLE_TO_)" code)
          (contains? #{"EPROTO" "DEPTH_ZERO_SELF_SIGNED_CERT"
                       "SELF_SIGNED_CERT_IN_CHAIN" "HOSTNAME_MISMATCH"}
                     code))
      (limits/tls-failure url e (.-message e))

      :else
      e)))

(defn- send-following!
  "A promise of the answer to `request` before its body is read, the
  redirects before it and the last request, as a vector. It follows the
  redirects until the instant `deadline` and checks each URL with
  `allow-pred`, unless the :signal of the request aborted. A header value
  that node:http would refuse rejects with ::invalid-header, as on the
  JVM. The :prepare-fn of the request, when it has one, returns the
  request of each hop."
  [request deadline allow-pred]
  (letfn [(step [request chain]
            (.then (redirect/hop request)
                   #(send-hop (update % :url url/legal-url) chain)))
          (send-hop [{:keys [url headers body signal] :as request} chain]
            (-> (if (limits/stopped? signal)
                  (js/Promise.reject (limits/aborted url))
                  (checked! url allow-pred deadline))
                (.then (fn [addresses]
                         (when (limits/unsendable-header headers)
                           (throw (limits/invalid-header url headers)))
                         (when (coll? body)
                           (throw (limits/invalid-body url)))
                         (open! request addresses deadline
                                (fn [] (limits/late url)))))
                (.catch #(throw (connect-failure url %)))
                (.then (fn [{:keys [status headers] :as opened}]
                         (let [location (headers/header headers "location")
                               limit    (:max-redirects request
                                                        redirect/limit)
                               target   (when (and (redirect/statuses status)
                                                   (< (count chain) limit))
                                          (redirect/target url location))]
                           (if-not target
                             [opened chain request]
                             (do (close! opened)
                                 (step (redirect/next-request request status
                                                              target)
                                       (conj chain
                                             [status url headers])))))))))]
    ;; so that Node's own parser, which parses the URL again, gets the host
    ;; that was checked
    (step (update request :url url/legal-url) [])))

(defn- answer
  "A promise of the response map for the answer that node:http `opened`
  for the `request`, which came after the redirects `chain`. The body of a
  success goes to the :sink of the request, when it has one."
  [{:keys [status headers] :as opened} chain
   {:keys [method url as range max-bytes sink truncate?] :as request}]
  (let [received (headers/content-range (headers/header headers
                                                        "content-range"))
        gzipped  (headers/gzipped? headers)
        n        (limits/prefix-length status (:start received) gzipped range
                                       max-bytes)
        moved    (redirect/permanent-url chain url)
        sunk?    (and sink (not= :head method) (<= 200 status 299))
        inflate  (fn [b]
                   (if gzipped
                     (or (gunzip b max-bytes)
                         (throw (limits/too-large url max-bytes)))
                     b))
        too-big  #(limits/too-large url max-bytes)
        ;; a body to cut at :max-bytes streams through the gunzip, one byte
        ;; further, to tell a body cut short from one that just fits
        cut      (fn []
                   (let [chunks (array)]
                     (.then (pour! opened (inc max-bytes) nil gzipped
                                   #(.push chunks %) too-big)
                            (fn [_]
                              (let [b    (.concat js/Buffer chunks)
                                    cut? (> (.-length b) max-bytes)]
                                {:bytes      (js/Uint8Array.
                                              (if cut?
                                                (.subarray b 0 max-bytes)
                                                b))
                                 :inflated?  true
                                 :truncated? cut?})))))
        body     (fn [b] {:bytes b})]
    (-> (cond
          ;; an answer to HEAD has no content by RFC 9110, section 9.3.2
          (= :head method)       (do (close! opened)
                                     (js/Promise.resolve
                                      (body (js/Uint8Array. 0))))
          sunk?                  (pour! opened n max-bytes gzipped sink too-big)
          n                      (.then (read! opened n nil nil) body)
          (and truncate?
               max-bytes)        (cut)
          :else                  (.then (read! opened nil max-bytes too-big)
                                        body))
        (.then (fn [got]
                 (let [{:keys [bytes inflated? truncated?]} (when-not sunk?
                                                               got)]
                   (cond-> {:status  status
                            :url     url
                            :headers headers
                            :body    (when (and bytes
                                                (not= 304 status)
                                                (pos? (.-length bytes)))
                                       (let [b (if inflated?
                                                 bytes
                                                 (inflate bytes))]
                                         (if (= :bytes as)
                                           b
                                           (encoding/decode b headers))))}
                     sunk?       (assoc :received got)
                     moved       (assoc :permanent-url moved)
                     (seq chain) (assoc :redirects chain)
                     received    (assoc :content-range received)
                     truncated?  (assoc :truncated? true))))))))

(defn send!
  "A promise of the response to `request`, sent with node:http within
  `timeout-seconds`, to URLs that `allow-pred` takes.

  It accepts gzip unless the headers name an Accept-Encoding."
  [{:keys [headers] :as request} timeout-seconds allow-pred]
  (let [deadline (+ (js/Date.now) (* 1000 timeout-seconds))
        named?   (some #(= "accept-encoding" (str/lower-case (str %)))
                       (keys headers))
        request  (cond-> request
                   (not named?) (assoc-in [:headers "Accept-Encoding"]
                                          "gzip"))]
    (.then (send-following! request deadline allow-pred)
           (fn [[opened chain last-request]]
             (answer opened chain last-request)))))
