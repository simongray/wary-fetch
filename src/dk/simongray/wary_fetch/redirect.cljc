(ns ^:no-doc dk.simongray.wary-fetch.redirect
  "Redirects, by RFC 9110, section 15.4: which ones a client follows,
  where each leads, the request that follows it, and where a chain of them
  moved a request to for good."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.url :as url]))

(def statuses
  "The statuses of the redirects that a client follows."
  #{301 302 303 307 308})

(def limit
  "How many redirects a client follows at most."
  5)

(def credential-headers
  "The names of the headers that hold credentials, in lower case."
  #{"authorization" "cookie" "proxy-authorization"})

(defn target
  "Where a redirect of the request to `url` with the Location `location`
  leads, or nil when it names no http or https URL with a host. It's nil
  too when it leaves https for http, which the client of the JDK refuses
  as well."
  [url location]
  (when-let [to (some->> location (url/absolute-url url))]
    (when (and (url/web-url? to)
               (not (and (= "https" (url/scheme url))
                         (= "http" (url/scheme to)))))
      to)))

(defn next-request
  "The request that follows `request` after a redirect of `status` to the
  URL `target`.

  A 303, or a 301 or 302 to a POST, turns into a GET without a body, as
  browsers and the JDK do. A HEAD stays a HEAD. Credentials stay behind
  when the redirect leaves the origin."
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

(defn permanent-url
  "The URL that the first request moved to for good, by the redirects in
  `chain` before `final-url`, or nil when the first isn't permanent.

  A redirect is permanent when it's a 301 or a 308, and the URL is the one
  that the permanent redirects at the start of the chain lead to."
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
