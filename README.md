wary-fetch
==========

[![Clojars Project](https://img.shields.io/clojars/v/dk.simongray/wary-fetch.svg)](https://clojars.org/dk.simongray/wary-fetch)

This is a Clojure and ClojureScript library for fetching URLs that
strangers give you, e.g. a feed URL that a user types in or a link in a
document you fetched. Requests and responses are plain maps.

- **Only the public internet, if you ask.** A stranger's URL can't reach
  your own machine or your local network, not even by a redirect.
- **Limits on every exchange.** Neither a slow server nor a gzip bomb can
  hold you up.
- **Secrets that don't leak.** Passwords and tokens stay out of your
  logs, and they don't follow a redirect to another origin.
- **Polling without waste.** A document that hasn't changed costs next
  to nothing to check, and the response says when to check again.

The same code runs on the JVM, in Node and in the browser, where a proxy
on your own server lets a page fetch from sites that don't allow it. It
depends only on [whatwg-url](https://github.com/simongray/whatwg-url) and
[whatwg-mime](https://github.com/simongray/whatwg-mime), which parse URLs
and MIME types the way browsers do, and on
[webcrypto-clj](https://github.com/simongray/webcrypto-clj) for bytes and
their encodings.

> This library was spun out of [podcast-clj](https://github.com/simongray/podcast-clj),
> a library for making podcast software, where it fetches feeds, media
> files and the rest of what a listener subscribes to. Like podcast-clj,
> it was developed with assistance from frontier LLMs.

Getting started
---------------

It requires Clojure 1.11+ and Java 11+, or Node 20.16+ or 22.3+. Add it from
[Clojars](https://clojars.org/dk.simongray/wary-fetch) to the `:deps` in
your `deps.edn`:

```clojure
dk.simongray/wary-fetch {:mvn/version "0.15.2"}
```

For unreleased changes, depend on the latest commit on `master` instead.
In shadow-cljs, a Git dependency only works through `deps.edn`, so set
`:deps true` in your `shadow-cljs.edn` too.

Fetching a document
-------------------

Fetch a document with `http/fetch!`:

```clojure
(require '[dk.simongray.wary-fetch :as http])

(def response
  (http/fetch! "https://example.com/"))

(select-keys response [:status :url :last-modified])
;; => {:status        200
;;     :url           "https://example.com/"
;;     :last-modified "Fri, 09 Oct 2026 20:19:47 GMT"}
```

The response also has the headers, and the body as text, decoded by the
charset that the server or the document declares. Each header name is in
lower case, with a vector of its values, so read a header with
`headers/header`. In ClojureScript, you get a promise of the response.

A URL on your own machine or network gets no request. It throws
instead:

```clojure
(http/fetch! "http://169.254.169.254/latest/meta-data")
;; throws "The request to http://169.254.169.254/latest/meta-data is not allowed"
```

The check is the `:allow-pred` option, a predicate of the URL and of each
redirect, which is `url/public?` unless you pass another. To fetch from
your own network, pass one that allows it, e.g. `(constantly true)`.

Polling
-------

Give the next request the `:etag` and `:last-modified` of the response,
and a document that hasn't changed comes back as a 304 without a body:

```clojure
(:status (http/fetch! "https://example.com/"
                      (select-keys response [:etag :last-modified])))
;; => 304
```

When the server says how long to wait, in a Retry-After, Cache-Control or
Expires header, the response of `http/fetch!` also has the instant to
ask again as `:not-before`. To get that instant from the headers of any
response, use `headers/not-before`:

```clojure
(require '[dk.simongray.wary-fetch.headers :as headers])

(headers/not-before {"cache-control" "max-age=600"}
                    #inst "2026-10-10T12:00:00Z")
;; => #inst "2026-10-10T12:10:00.000-00:00"
```

The wait is as long as the server says. To poll a server that might be
set up wrong, cap it with the `:max-wait-seconds` option of either
function, e.g. 3600 for an hour.

The `headers` namespace parses the other headers of a response too, e.g.
a Link header with `headers/links`. For a Content-Type, use whatwg-mime.

Requests
--------

A request is a map of `:method`, `:url`, `:headers` and `:body`, which
you send with `http/send!`. It can also have these keys:

- `:timeout-seconds`, the limit of the whole exchange, 30 by default
- `:max-bytes`, the limit of the body, 100 MB by default
- `:truncate?`, true to cut a body over that limit rather than throw,
  which marks the response `:truncated?`
- `:max-redirects`, how many redirects to follow, 5 by default
- `:allow-pred`, a predicate of the URL and of each redirect
- `:user-agent`, the User-Agent, unless the headers name one
- `:as`, either `:bytes` for a body of bytes, e.g. of an image, or
  `:json` for the value of a JSON body
- `:range`, the first and the last byte to ask for
- `:sink`, to take the body in pieces, see [Media files](#media-files)
- `:signal`, to stop the exchange, e.g. when the user cancels: an
  AbortSignal in ClojureScript, or on the JVM an atom that turns true
- `:prepare-fn`, a function that returns the request to send for each
  hop (the first and each redirect), e.g. to sign each hop for its own
  URL as [httpsig-clj](https://github.com/simongray/httpsig-clj) does

A request to any URL goes through unless its `:allow-pred` says
otherwise. To send one to the public internet only, as `http/fetch!`
does, use `http/send-public!`. An answer with an error status, e.g. a
404, is a response like any other.

The functions that make a request for you take its limits,
`:allow-pred` and `:user-agent` as options. To put the same options on
a request of your own, use `http/with-options`. Besides those,
`http/fetch!` takes `:credentials`, `:accept` and a `:send` to use in
place of `http/send!`, e.g. one that sends through [the
proxy](#the-proxy).

### Posting data

The body of a request is text or bytes. To make one from data, build the
request with one of these:

- `http/form-request` for a form, e.g. to an OAuth token endpoint. It
  hides the body, since a form often holds a secret.
- `http/json-request` for a value as JSON.
- `http/multipart-request` for files beside the fields of a form, e.g. a
  photo to upload.

A map or another collection as the body throws rather than going out as
its printed text. The `json` namespace reads and writes JSON on every
platform, with no library to add. For the hashes, HMACs and random bytes
that signatures and tokens need, use webcrypto-clj.

Credentials
-----------

Hide a password or a token with `secret/hide`, and it prints as
`#<secret>`:

```clojure
(require '[dk.simongray.wary-fetch.secret :as secret])

(def credentials
  {:username "me" :password (secret/hide "hunter2")})

credentials
;; => {:username "me", :password #<secret>}

(http/fetch! "https://example.com/private.xml" {:credentials credentials})
```

The request's Authorization header is hidden too, and it stays behind
when a redirect leaves the origin. To send the request with another HTTP
client, reveal its hidden headers and body with `http/revealed`.

A token that you read from a file often ends with a line break, which
HTTP doesn't allow in a header, so trim it first. Otherwise the request
throws, and the exception names the header but not the token.

Failures
--------

A request that fails throws an ex-info (in ClojureScript it rejects with
one). Its `:type` says why, as a keyword in the `dk.simongray.wary-fetch`
namespace:

- **refused**: the `:allow-pred` refused the URL or a redirect.
- **timeout** or **too-large**: the exchange took longer than its
  `:timeout-seconds`, or the body held more than its `:max-bytes`.
- **aborted**: the `:signal` stopped the exchange.
- **unresolved**, **unreachable** or **tls**: the host's name doesn't
  resolve, the host can't be reached or the secure connection failed
  (e.g. over an expired certificate). So you can tell a dead network
  from a dead server, and a dead server from a bad certificate.
- **invalid-header** or **invalid-body**: the request can't be sent as
  it is, e.g. with a line break in a header or a map as the body.

A browser doesn't report why a request failed. When it reports the
device as offline, the type is unreachable.

To get the exception in a map rather than as a throw, use
`http/try-send!`. On the JVM it returns the map in a future, so that
several requests can be under way at once. To turn an exception into
data, use `http/failure`. To check for a timeout, including one from
the platform's own client, use `http/timeout?`.

Media files
-----------

To ask about a large file without downloading it, use `http/probe!`,
which sends a HEAD request and a request for the first two bytes:

```clojure
(http/probe! "https://upload.wikimedia.org/wikipedia/commons/c/c8/Example.ogg")
;; => {:head {:status 200
;;            :type   "application/ogg"
;;            :length 104793
;;            …}
;;     :get  {:status 206
;;            :range  {:start 0 :end 1 :total 104793}
;;            …}}
```

To download a file without holding it in memory, give the request a
`:sink`, which can be:

- a function of the bytes of each piece, which in ClojureScript can
  return a promise to wait for before the next piece
- an OutputStream on the JVM
- a writable stream in Node

The response then has no body, but the number of bytes that the sink
`:received`:

```clojure
(require '[clojure.java.io :as io])

(with-open [out (io/output-stream "Example.ogg")]
  (:received
   (http/send-public!
    {:url       "https://upload.wikimedia.org/wikipedia/commons/c/c8/Example.ogg"
     :sink      out
     :max-bytes nil})))
;; => 104793
```

The limits apply to the sink too, so the example sets `:max-bytes` to
nil for a file that can be larger than 100 MB. Only the body of a
success goes to the sink, so a 404 still has its page as the body.

To resume a download that broke off, ask for the rest with a `:range`
and an If-Range header, which `headers/if-range` makes from the headers
of the first answer. It returns nil when those headers can't show that
the file is unchanged. In that case, start the download again. To read
a file in parts by byte range, use `http/fetcher`. On the JVM,
`http/digest!` computes a SHA-256 or another digest of a file as it
streams past.

The proxy
---------

A web page can only read an answer from another origin when that origin
allows it with CORS headers, and most don't. To fetch from them anyway,
make a Ring handler with `proxy/handler` and mount it on your own server
(behind your login) at a route such as `/proxy`:

```clojure
(require '[dk.simongray.wary-fetch.proxy :as proxy])

(def proxy-handler
  (proxy/handler))
```

The handler fetches the URL in its `url` parameter when that URL is on
the public internet, and answers with what came back. In the page, send
each request through it with `proxy/sender` to get the response that a
direct request would return:

```clojure
(http/fetch! "https://example.com/feed.xml"
             {:send (proxy/sender "/proxy")})
```

The options of the handler, e.g. which headers it passes on, are in
`proxy/defaults`.

The handler holds each body in memory, so it passes on 32 MB at most.
For a large file, e.g. an episode that a page saves, mount
`proxy/stream-handler` as well. It passes the body on as it arrives, and
a Range header too, so that a download can resume. Its options are in
`proxy/stream-defaults`.

In ClojureScript
----------------

The functions that send return promises in ClojureScript. To write code
for both platforms at once, chain them with `async/then`, which takes a
value, a future or a promise alike. The `async` namespace has a few more
functions like it.

### Node

In Node, requests go through node:http, with the same checks as on the
JVM. The library looks up the host's name before it calls the
`:allow-pred`, so that `url/public?` checks every address of the name.
It then connects only to those addresses. It also follows redirects
itself and calls the `:allow-pred` on each.

### Browsers

In a browser, requests go through fetch, and two of the checks are
weaker:

- A page can't look up a name, so `url/public?` checks only the name
  itself and refuses e.g. localhost and names under .local.
- Fetch follows redirects itself, so the `:allow-pred` never gets their
  URLs.

Those checks matter less in a page, since its requests come from the
user's own device rather than from your server. CORS also hides the
headers that a server doesn't expose, and the response then has
`:filtered?`.

### Capacitor

In a Capacitor app, CORS applies to the web view too, so send through the
native HTTP of the CapacitorHttp plugin instead. It makes the same
checks as in Node, except that it doesn't look up names:

```clojure
(require '["@capacitor/core" :refer [CapacitorHttp]]
         '[dk.simongray.wary-fetch.capacitor :as capacitor])

(http/fetch! "https://example.com/feed.xml"
             {:send (capacitor/sender CapacitorHttp)})
```

The plugin hands over each body whole, so leave large files, e.g.
episodes, to a plugin for file transfers.

### React Native

In React Native, requests go through fetch, and no CORS limits what it
reaches. So the library follows redirects itself and calls the
`:allow-pred` on each, as in Node, but like a page it can't look up
names. Expo's fetch
streams the body, so `:max-bytes` stops a large one as it arrives, while
React Native's own fetch hands the body over whole.

### Electron

In an Electron app, the page sends its requests through the main process,
where Node sends them with all its checks. Give the main process
`electron/handler` and the page `electron/sender`, as the docstring of
the `electron` namespace shows. The handler applies its own limits
whatever a page asks for, so a page can't reach the computer's own
network.

Principles
----------

- **Standards first.** The code follows RFC 9110, RFC 9111 and the other
  specs it implements, and cites their sections.
- **Plain data.** Requests and responses are maps, so you can send a
  request with any HTTP client, or with a function of your own.
- **One codebase.** Apart from each platform's client, the library is
  written in `.cljc` and behaves the same everywhere, as far as
  JavaScript allows. It parses each URL and its host with whatwg-url, so
  it checks a host that a browser would parse as an IP address (e.g.
  0x7f.1) as one.

Development
-----------

```bash
clojure -X:test                       # the tests on the JVM
npm install                           # once, for ClojureScript
clojure -M:cljs compile test          # the tests in Node
clojure -M:cljs compile browser-test  # the tests in a browser
node test/browser-run.mjs             # run them in a headless Chrome
```

Every namespace is public except those marked `^:no-doc`: each
platform's client, the rules that the clients share and a few helpers.

License
-------

The wary-fetch project is licensed under the [MIT licence](LICENSE).
