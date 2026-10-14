(ns dk.simongray.wary-fetch.electron
  "A :send for the page of an Electron app, which sends each request
  through the main process, where Node sends it and CORS doesn't stop it.

  The main process answers with handler, and the page sends with sender
  over Electron's IPC, which the preload script exposes to the page:

      ;; main process
      (.handle ipcMain \"wary-fetch\" (electron/handler))

      ;; preload script
      (.exposeInMainWorld contextBridge \"waryFetch\"
                          #js {:send #(.invoke ipcRenderer \"wary-fetch\" %)})

      ;; page
      (def send (electron/sender (.. js/window -waryFetch -send)))

  The main process applies its own limits, so a page can't reach what the
  handler doesn't allow, e.g. the app's own network. A request and its
  response cross the IPC as plain data without functions, so a page's
  :sink or :allow-pred stays in the page."
  (:require [dk.simongray.wary-fetch :as http]
            [dk.simongray.wary-fetch.async :as async]
            [dk.simongray.wary-fetch.json :as json]
            [dk.simongray.wary-fetch.limits :as limits]
            [dk.simongray.wary-fetch.redirect :as redirect]
            [dk.simongray.wary-fetch.url :as url]))

(def ^:no-doc request-keys
  "The keys of a request that cross to the main process."
  [:method :url :headers :body :as :range :timeout-seconds :max-bytes
   :truncate? :max-redirects :query-params :user-agent])

(def ^:no-doc response-keys
  "The keys of a response that cross back to the page, with the :headers
  and the :redirects as data."
  [:status :url :headers :body :content-range :permanent-url :redirects
   :truncated?])

(defn- plain
  "The map `m` with the keys `ks` that it has, as plain data for the IPC:
  keywords as text, and bytes as they are."
  [m ks]
  (clj->js (into {}
                 (keep (fn [k]
                         (when (some? (get m k))
                           [(name k) (let [v (get m k)]
                                       (if (keyword? v) (name v) v))])))
                 ks)))

(defn- read-plain
  "The map of the plain data `o` from the IPC, with the keys `ks`, and
  keywords for the values of the keys `keywords`. Bytes stay bytes, and
  the :content-range is a map of keywords."
  [^js o ks keywords]
  (into {}
        (keep (fn [k]
                (let [v (aget o (name k))]
                  (when (some? v)
                    [k (cond
                         (contains? keywords k)      (keyword v)
                         (instance? js/Uint8Array v) v
                         (= :content-range k)        (js->clj v :keywordize-keys true)
                         :else                       (js->clj v))]))))
        ks))

(defn- failure
  "The plain data of the exception `e`, which crosses the IPC as an object
  rather than as an Error, so that its :type and :url stay."
  [e]
  (let [{:keys [type url]} (ex-data e)]
    #js {:error #js {:message (ex-message e)
                     :type    (some-> type str (subs 1))
                     :url     url}}))

(defn handler
  "A function to pass to ipcMain.handle in the main process, which sends
  each request of a page with the :send and within the limits of `opts`,
  and returns a promise of the response as plain data.

  The `opts` set what the main process allows:

  - :send, the function that sends, http/send! by default
  - :allow-pred, the URLs that a page may reach, url/public? by default,
    whatever a page asks
  - :max-bytes and :timeout-seconds, the most that a page may ask for, by
    default those of send!"
  ([]
   (handler {}))
  ([{:keys [send allow-pred max-bytes timeout-seconds]
     :or   {send            http/send!
            allow-pred      url/public?
            max-bytes       (* 100 1024 1024)
            timeout-seconds 30}
     :as   opts}]
   (fn [_event ^js asked]
     (let [asked   (read-plain asked request-keys #{:method :as})
           request (-> (dissoc asked :as)
                       (cond-> (= :bytes (:as asked)) (assoc :as :bytes))
                       (update :max-bytes #(min max-bytes (or % max-bytes)))
                       (update :timeout-seconds
                               #(min timeout-seconds (or % timeout-seconds)))
                       (assoc :allow-pred allow-pred))]
       (-> (async/attempt #(send request))
           (async/then (fn [{:keys [value error]}]
                         (if error
                           (failure error)
                           (plain value response-keys)))))))))

(defn- response-of
  "The response in the plain data `o` from the main process. When `o`
  names an exception instead, this throws it with its :type and :url."
  [^js o]
  (if-let [^js e (.-error o)]
    (throw (ex-info (.-message e)
                    (cond-> {}
                      (.-type e) (assoc :type (keyword (.-type e)))
                      (.-url e)  (assoc :url (.-url e)))))
    (read-plain o response-keys #{})))

(defn sender
  "A function to pass as :send in the page, which sends each request
  through the main process with `invoke`.

  The `invoke` function passes the plain data of a request to the main
  process's handler and returns a promise of its answer, e.g.
  ipcRenderer.invoke with the name of the channel. The keys of a request
  are those of send!, except :sink, :allow-pred and :signal, which the
  main process doesn't get. The sender checks the :signal before the
  request crosses. Hidden headers and bodies cross as text, as they go to
  a server. The main process follows the redirects, so the :prepare-fn of
  the request runs in the page, on the first hop alone."
  [invoke]
  (fn [{:keys [url as signal prepare-fn] :as request}]
    (if (limits/stopped? signal)
      (js/Promise.reject (limits/aborted url))
      (-> (if prepare-fn
            ;; with its query in its URL, as the main process sends it
            (redirect/hop (http/prepared request))
            (js/Promise.resolve (http/revealed request)))
          (.then #(invoke (plain (update % :as (fn [a] (when (= :bytes a) :bytes)))
                                 request-keys)))
          (.then response-of)
          (.then (fn [response]
                   (cond-> response
                     (= :json as) (update :body json/read-json))))))))
