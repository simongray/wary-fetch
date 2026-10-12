(ns dk.simongray.wary-fetch.url
  "URLs, and the hosts that they name, the same way on both platforms.

  This covers percent-encoding and query strings, a URL made absolute
  against the document that it's in, as a browser does it, and the scheme
  and the origin of a URL. It also tells whether the host of a URL is on
  the public internet, which is what a request that a stranger chose must
  check.

  URLs are parsed by whatwg-url, which implements the URL Standard, so a
  host is read as a browser reads it, e.g. 0x7f.1 as 127.0.0.1."
  (:require [clojure.string :as str]
            [dk.simongray.wary-fetch.secret :as secret]
            [dk.simongray.whatwg-url :as whatwg-url]
            [dk.simongray.whatwg-url.hosts :as hosts]
            [dk.simongray.whatwg-url.percent :as percent])
  #?(:clj (:import [java.net InetAddress]
                   [java.nio.charset StandardCharsets])))

(defn percent-encode
  "The string `s` percent-encoded for a query string, the same way on both
  platforms. Spaces become %20, not +, and letters, digits and the marks
  - _ . ! ~ * ' ( ) stay as they are."
  [s]
  (percent/encode (str s)))

(defn percent-decode
  "The string `s` with its percent-encoding undone. A plus sign stays a
  plus sign, a % that starts no escape stays as it is, and bytes that
  aren't UTF-8 become U+FFFD, rather than throwing."
  [s]
  (percent/decode (str s)))

(defn query-string
  "The query string for `params`, with keys and values URL-encoded and nil
  values left out. A true value becomes the bare flag that some APIs take."
  [params]
  (str/join "&" (for [[k v] params
                      :when (some? v)]
                  (if (true? v)
                    (percent-encode (name k))
                    (str (percent-encode (name k)) "=" (percent-encode v))))))

(defn query-pairs
  "The parameters of the query string `s`, decoded, as pairs of a name and
  a value in their order, a name that comes twice twice. A plus sign is a
  space, as in the body of a form, and a name without = has a nil value.

  Use this rather than query-params where a repeated parameter must be
  refused, as RFC 6749, section 3.1, has an OAuth server do."
  [s]
  (let [decode #(percent-decode (str/replace (str %) "+" " "))]
    (vec (for [part  (str/split (str s) #"&")
               :let  [[k v] (str/split part #"=" 2)
                      k     (decode k)]
               :when (seq k)]
           [k (some-> v decode)]))))

(defn query-params
  "The parameters of the query string `s`, decoded, by name. A plus sign
  is a space, as in the body of a form. Of a name that comes twice, the
  last value counts."
  [s]
  (into {} (query-pairs s)))

(defn with-query
  "The `url` with `params` added to its query, encoded as in a query
  string. The parameters go after those that the URL has, and before its
  fragment."
  [url params]
  (let [[_ base fragment] (re-matches #"(?s)([^#]*)(#.*)?" (str url))
        query             (query-string params)
        joint             (cond
                            (not (str/includes? base "?")) "?"
                            (re-find #"[?&]$" base)        ""
                            :else                          "&")]
    (if (str/blank? query)
      (str url)
      (str base joint query fragment))))

(defn without-trailing-slashes
  "The `url` without the slashes at its end, e.g. the root of an API that
  paths are joined to, in a time that grows with its length alone."
  [url]
  ;; a match may start only where a run of slashes starts: /+$ alone is
  ;; tried again from every slash of a long run, quadratically
  (str/replace (str url) #"(^|[^/])/+$" "$1"))

(defn split-credentials
  "The `url` without the user name and the password that it holds, and
  those as credentials for authorization, as a pair. The credentials are
  nil when the URL holds none.

  A URL such as https://user:pass@host/feed.xml holds them. Use this when
  you keep a URL, to keep the password apart from the address, since an
  address travels into files, logs and messages.

  The password is a hidden text, which the printer doesn't show."
  [url]
  (let [[_ scheme authority tail]
        (re-matches #"(?s)([a-zA-Z][a-zA-Z0-9+.-]*://)([^/?#]*)(.*)"
                    (str url))

        at (some-> authority (str/last-index-of "@"))]
    (if at
      (let [[user password] (str/split (subs authority 0 at) #":" 2)]
        [(str scheme (subs authority (inc at)) tail)
         (when-not (str/blank? user)
           {:username (percent-decode user)
            :password (secret/hide (percent-decode (or password "")))})])
      [url nil])))

(def ^:no-doc legal-url-characters
  "The ASCII characters that a URI may contain as they are, and the
  percent sign of an escape. They're the unreserved and reserved sets of
  RFC 3986, sections 2.3 and 2.2."
  (set (str "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"
            ":/?#[]@!$&'()*+,;="
            "%")))

#?(:clj
   (defn- encode-illegal
     "The part `s` of a URL with each character that a URI may not contain
     percent-encoded as UTF-8, and the characters of the set `also` too. A
     percent sign that starts no escape is encoded as well."
     [^String s also]
     (let [sb     (StringBuilder.)
           bytes  (.getBytes s StandardCharsets/UTF_8)
           n      (alength bytes)
           digit? (fn [i]
                    (and (< i n)
                         (let [b (aget bytes i)]
                           (or (<= 48 b 57)
                               (<= 65 b 70)
                               (<= 97 b 102)))))]
       (dotimes [i n]
         (let [b (bit-and (aget bytes i) 0xFF)
               c (char b)]
           (cond
             (and (= \% c)
                  (not (and (digit? (inc i)) (digit? (+ i 2)))))
             (.append sb "%25")

             (and (< b 128)
                  (legal-url-characters c)
                  (not (also c)))
             (.append sb c)

             :else
             (.append sb (format "%%%02X" b)))))
       (str sb))))

(defn legal-url
  "The `url` as the URL Standard writes it, which is what the client of
  each platform gets, so that it reads the host that public? checked. On
  the JVM each character that java.net.URI can't take is percent-encoded
  as UTF-8 too, e.g. a bracket in the path, or a percent sign that starts
  no escape.

  A URL that's written so already comes back unchanged. A host with
  letters outside ASCII gets the ASCII form of IDNA. Text that isn't a
  URL stays as it is, but for the encoding on the JVM."
  [^String url]
  (let [url (or (whatwg-url/href url) url)]
    #?(:clj  (let [[_ head path tail]
                   (re-matches #"(?s)([a-zA-Z][a-zA-Z0-9+.-]*://[^/?#]*)?([^?#]*)(.*)"
                               url)]
               (str (encode-illegal (str head) #{})
                    (encode-illegal path #{\[ \]})
                    (encode-illegal tail #{})))
       :cljs url)))

(defn absolute-url
  "The `url` made absolute against `base`, the URL of the document that
  it's in, as a browser does it. Nil when one of them doesn't read as a
  URL.

  A path, a path from the root, a query alone or a URL without a scheme
  resolves against the base. An absolute URL stays as it is.

  A base with an opaque path, such as a mailto URL, takes only a fragment.
  An empty `url` names the document itself. A path doesn't go above the
  root, so ../x in a document at the root is /x."
  [base url]
  (when (and base url)
    (whatwg-url/resolve (str base) (str url))))

;; URL Standard, section 4.4, the basic URL parser
(defn scheme
  "The scheme of the URL `s` in lower case, or nil for a relative URL. It's
  read as a browser reads it: without the tabs and line breaks in it, and
  without the spaces and control characters in front."
  [s]
  (let [s (-> (str s)
              (str/replace #"[\t\n\r]" "")
              (str/replace #"^[\x00-\x20]+" ""))]
    (some-> (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*(?=:)" s) str/lower-case)))

;; RFC 3986, appendix B, with a scheme as section 3.1 has it
(def ^:no-doc parts-pattern
  "The scheme, the authority, the path, the query and the fragment of a
  URL, as groups."
  (re-pattern (str "(?s)([a-zA-Z][a-zA-Z0-9+.\\-]*):"
                   "(?://([^/?#]*))?([^?#]*)(?:\\?([^#]*))?(?:#(.*))?")))

;; RFC 3986, section 3.2
(defn parts
  "The parts of the URL `s` as it's written, before a parser tidies them,
  as a map of :scheme, :authority, :userinfo, :host, :port, :path, :query
  and :fragment, each nil when the URL has none, or nil when `s` has no
  scheme. The scheme is in lower case, and the rest is as written, e.g. a
  port of 443 for https, or a .. segment in the path, percent-encoded or
  not, for a check of the URL rather than a request."
  [s]
  (when-let [[_ scheme authority path query fragment]
             (re-matches parts-pattern (str s))]
    (let [[_ userinfo host port]
          (some->> authority
                   (re-matches #"(?s)(?:(.*)@)?(\[[^\]]*\]|[^:]*)(?::(.*))?"))]
      {:scheme    (str/lower-case scheme)
       :authority authority
       :userinfo  userinfo
       :host      host
       :port      port
       :path      path
       :query     query
       :fragment  fragment})))

(def ^:no-doc default-ports
  "The port of each scheme of the web, for a URL that names none."
  {"http" 80 "https" 443})

(defn origin
  "The scheme, host and port of the http or https URL `url` as a vector,
  as the URL standard and browsers read them. Nil for another URL, and for
  a URL without a host.

  The host is in lower case, and a name with letters outside ASCII is in
  the ASCII form of IDNA. When the URL names no port, the port is the one
  of the scheme."
  [url]
  (when-let [{:keys [scheme host port]} (some-> url str whatwg-url/parse)]
    (when (and (contains? default-ports scheme) (seq host))
      [scheme host (or port (default-ports scheme))])))

(defn host
  "The host of the http or https URL `url`, in lower case."
  [url]
  (second (origin url)))

(defn web-url?
  "Whether `s` is an http or https URL with a host."
  [s]
  (boolean (and (string? s)
                (re-find #"(?i)^https?://" s)
                (origin s))))

(defn same-origin?
  "Whether the URLs `a` and `b` have the same scheme, host and port. False
  when a URL has no host."
  [a b]
  (let [o (origin a)]
    (and (some? o) (= o (origin b)))))

;; IANA IPv4 Special-Purpose Address Registry (RFC 6890), as of 2025-10-09:
;; the blocks that are not globally reachable, and multicast
(defn ^:no-doc public-ipv4?
  "Whether the IPv4 `address`, a vector of four numbers, is on the public
  internet, by the IANA registry of special addresses.

  It isn't when it's in this network, private, shared, loopback,
  link-local, multicast or reserved, or kept for protocols, tests and
  documentation."
  [[a b c _ :as address]]
  (not (or (= 0 a)
           (= 10 a)
           (and (= 100 a) (<= 64 b 127))
           (= 127 a)
           (and (= 169 a) (= 254 b))
           (and (= 172 a) (<= 16 b 31))
           (and (= 192 a) (= 0 b) (contains? #{0 2} c))
           (and (= 192 a) (= 88 b) (= 99 c))
           (and (= 192 a) (= 168 b))
           (and (= 198 a) (contains? #{18 19} b))
           (and (= 198 a) (= 51 b) (= 100 c))
           (and (= 203 a) (= 0 b) (= 113 c))
           (<= 224 a))))

;; IANA IPv6 Special-Purpose Address Registry (RFC 6890), as of 2025-10-09:
;; the blocks that are not globally reachable, and multicast
(defn ^:no-doc public-ipv6?
  "Whether the IPv6 address of the sixteen numbers `b` is on the public
  internet, by the IANA registry of special addresses.

  It isn't when it's unspecified, loopback, unique local, link-local,
  site-local or multicast, or kept for documentation, benchmarks,
  discard, segment routing or Teredo. Nor is an address that holds an
  IPv4 address that isn't public."
  [b]
  (let [word     #(+ (* 256 (nth b %)) (nth b (inc %)))
        ipv4-at  #(public-ipv4? (subvec b % (+ % 4)))
        zeros?   #(every? zero? (subvec b 0 %))
        nat64?   (and (= 0x64 (word 0)) (= 0xff9b (word 2)))]
    (cond
      (zeros? 15)                                               false
      (zeros? 12)                                               (ipv4-at 12)
      (and (zeros? 10) (= 0xffff (word 10)))                    (ipv4-at 12)
      ;; 64:ff9b::/96 holds an IPv4 address, 64:ff9b:1::/48 is for local use
      (and nat64? (= 0 (word 4) (word 6) (word 8) (word 10)))   (ipv4-at 12)
      nat64?                                                    false
      (= 0x2002 (word 0))                                       (ipv4-at 2)
      (and (= 0x2001 (word 0)) (= 0 (word 2)))                  false
      (and (= 0x2001 (word 0)) (= 2 (word 2)) (= 0 (word 4)))   false
      (and (= 0x2001 (word 0)) (= 0xdb8 (word 2)))              false
      (and (= 0x3fff (word 0)) (< (nth b 2) 0x10))              false
      (= 0x5f00 (word 0))                                       false
      (and (= 0x100 (word 0)) (= 0 (word 2) (word 4))
           (<= (word 6) 1))                                     false
      (= 0xfc (bit-and 0xfe (nth b 0)))                         false
      (and (= 0xfe (nth b 0)) (<= 0x80 (nth b 1)))              false
      (= 0xff (nth b 0))                                        false
      :else                                                     true)))

(defn ^:no-doc public-ip?
  "Whether the IP address of the numbers `b`, four of IPv4 or sixteen of
  IPv6, is on the public internet, by the IANA registries of special
  addresses."
  [b]
  (if (= 4 (count b))
    (public-ipv4? b)
    (public-ipv6? b)))

#?(:clj
   (defn- public-address?
     "Whether the IP address `a` is on the public internet, both by the
     kinds of address of the JDK and by the IANA registries of special
     addresses."
     [^InetAddress a]
     (let [b (mapv #(bit-and % 0xff) (.getAddress a))]
       (and (not (or (.isAnyLocalAddress a)
                     (.isLoopbackAddress a)
                     (.isLinkLocalAddress a)
                     (.isSiteLocalAddress a)
                     (.isMulticastAddress a)))
            (public-ip? b)))))

(defn ^:no-doc ipv4
  "The four numbers of the IPv4 address that the text `s` writes, or nil
  for anything else."
  [s]
  (when (re-matches #"\d{1,3}(?:\.\d{1,3}){3}" s)
    (let [ns (mapv parse-long (str/split s #"\."))]
      (when (every? #(<= 0 % 255) ns)
        ns))))

(defn ip-address
  "The bytes of the IP address that the text `s` writes, as numbers, or
  nil for anything else, e.g. a name or a hash.

  An IPv4 address has four, and an IPv6 address sixteen. The text of an
  IPv6 address can have the brackets that a URL puts around it."
  [s]
  (when (string? s)
    (or (ipv4 s)
        (let [[_ bracketed bare] (re-matches #"\[(.*)\]|(.*:.*)" s)]
          (some->> (or bracketed bare)
                   hosts/ipv6
                   (into [] (mapcat (fn [piece] [(quot piece 256) (rem piece 256)]))))))))

;; RFC 6761, section 6.3 (localhost); RFC 6762, section 3 (.local);
;; RFC 8375 (.home.arpa); URL Standard, section 3.5, the ends in a number
;; checker
(defn public-name?
  "Whether the domain name `host` is a name of the public internet.

  Such a name has a dot in it, not counting a dot at its end. It also
  doesn't stay on a local network, as localhost, .local, .internal and
  .home.arpa do. A host whose last part is a number isn't a name but an
  IPv4 address, as a browser reads it, e.g. 10.0.0.1 or 0x7f.0.0.1."
  [host]
  (let [h (str/replace (str/lower-case (str host)) #"\.$" "")]
    (and (str/includes? h ".")
         (not (re-find #"(^|\.)(localhost|local|internal|home\.arpa)$" h))
         (not (hosts/ends-in-a-number? h)))))

(defn public-host?
  "Whether the host of the http or https URL `url` is on the public
  internet by its text alone, with no lookup of the name: an address of
  the public internet, or a name that public-name? takes.

  A name can still lead to an address on a local network, which public?
  checks on the JVM and in Node, so use this where a lookup can't be made
  or costs too much, e.g. to check many links at once."
  [url]
  (let [h (some-> (host url) (str/replace #"\.$" ""))]
    (boolean (and h
                  (if-let [b (ip-address h)]
                    (public-ip? b)
                    (public-name? h))))))

#?(:cljs
   (def ^:dynamic ^:no-doc *addresses*
     "The addresses that send! looked up in Node for the host of a request,
     while it asks the :allow-pred of the request: a map of the host to the
     numbers of each address, or nil for an address that doesn't read."
     {}))

(defn public?
  "Whether the host of the http or https URL `url` is on the public
  internet rather than on the network of the app or of its device.

  On the JVM each address that the name resolves to must be public, and a
  name that doesn't resolve isn't. In ClojureScript the host must pass
  public-host?, and when send! asks in Node, each address of the name must
  be public too."
  [url]
  #?(:clj  (try
             ;; the host as the URL Standard reads it, which legal-url
             ;; gives the client of the JDK that sends the request
             (let [h (host url)]
               (boolean (and h
                             ;; the JVM caches these for 30 seconds, so the
                             ;; request goes to the addresses checked here
                             (every? public-address?
                                     (InetAddress/getAllByName h)))))
             (catch Exception _
               false))
     :cljs (let [h (some-> (host url) (str/replace #"\.$" ""))]
             (boolean (and (public-host? url)
                           (or (ip-address h)
                               (every? #(and % (public-ip? %))
                                       (get *addresses* h))))))))

(comment
  (query-string {:q "clojure podcasts" :max 10 :pretty true})
  (split-credentials "https://ann:secret@example.org/feed.xml")
  (absolute-url "https://example.org/shows/feed.xml" "../art/cover.png")
  (origin "HTTPS://Example.org/feed.xml")
  (public? "http://192.168.1.10/feed.xml")
  #_.)
