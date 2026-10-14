(ns ^:no-doc dk.simongray.wary-fetch.redirect
  "Redirects by RFC 9110, section 15.4: which ones a client follows, where
  each leads and the request that follows it, and the URL that a chain of
  them moved a request to for good."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.url :as url]))

(def statuses
  "The statuses of the redirects that a client follows."
  #{301 302 303 307 308})

(def limit
  "How many redirects a client follows at most, unless a request's
  :max-redirects says otherwise."
  5)

(def credential-headers
  "The names of the headers that hold credentials, in lower case."
  #{"authorization" "cookie" "proxy-authorization"})

(defn target
  "Where a redirect from `url` with the Location `location` leads, or nil
  when it names no http or https URL with a host. It's also nil when the
  redirect leaves https for http, which the JDK's client refuses too."
  [url location]
  (when-let [to (some->> location (url/absolute-url url))]
    (when (and (url/web-url? to)
               (not (and (= "https" (url/scheme url))
                         (= "http" (url/scheme to)))))
      to)))

(defn next-request
  "The request that follows `request` after a redirect of `status` to the
  URL `target`.

  After a 303, or a 301 or 302 to a POST, the request becomes a GET
  without a body, as in browsers and the JDK. A HEAD stays a HEAD.
  Credentials stay behind when the redirect leaves the origin."
  [{:keys [method url headers body] :as request} status target]
  (let [get?        (or (= 303 status)
                        (and (#{301 302} status) (= :post method)))
        credential? (fn [[k _]]
                      (credential-headers (str/lower-case (str k))))]
    (assoc request
           :url     target
           :method  (if (and get? (not= :head method)) :get method)
           :headers (cond->> headers
                      (not (url/same-origin? url target))
                      (into {} (remove credential?)))
           :body    (when-not get? body))))

(defn hop
  "The request of a hop (the first or one after a redirect) as the
  :prepare-fn of the `request` returns it, e.g. signed for its URL, or the
  request itself when it has none (in ClojureScript a promise of it). On
  the JVM, it derefs a future that the function returns. The request
  keeps the function for the next hop."
  [{:keys [prepare-fn] :as request}]
  (let [keep-fn #(assoc % :prepare-fn prepare-fn)]
    #?(:clj  (if prepare-fn
               (let [r (prepare-fn request)]
                 (keep-fn (if (future? r) @r r)))
               request)
       :cljs (if prepare-fn
               (.then (js/Promise.resolve (prepare-fn request)) keep-fn)
               (js/Promise.resolve request)))))

(defn permanent-url
  "The URL that the first request moved to for good, by the redirects in
  `chain` before `final-url`, or nil when the first isn't permanent.

  A redirect is permanent when it's a 301 or a 308. The URL is where the
  permanent redirects at the start of the chain lead."
  [chain final-url]
  (let [n (count (take-while #(#{301 308} (first %)) chain))]
    (when (pos? n)
      (get (conj (mapv second chain) final-url) n))))

(comment
  ;; a POST that a 303 turns into a GET, and credentials that stay behind
  (next-request {:method  :post
                 :url     "https://a.example/x"
                 :headers {"Authorization" "Basic …"}
                 :body    "{}"}
                303
                "https://b.example/y")
  #_.)
