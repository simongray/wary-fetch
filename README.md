wary-fetch
==========

[![Clojars Project](https://img.shields.io/clojars/v/dk.simongray/wary-fetch.svg)](https://clojars.org/dk.simongray/wary-fetch)

This is a Clojure and ClojureScript library for fetching URLs that
strangers give you, e.g. a feed URL that a user types in, or a link in a
document you fetched. Requests and responses are plain maps.

- **Only the public internet, if you ask.** A stranger's URL can't reach
  your own machine or your local network, not even by a redirect.
- **Limits on every exchange.** Neither a slow server nor a gzip bomb can
  hold you up.
- **Secrets that don't leak.** Passwords and tokens stay out of your
  logs, and they don't follow a redirect to another site.
- **Polling without waste.** A document that hasn't changed costs next
  to nothing to check, and the response says when to check again.

The same code runs on the JVM, in Node and in the browser, where a proxy
on your own server lets a page fetch from sites that don't allow it.
The only dependencies are [whatwg-url](https://github.com/simongray/whatwg-url)
and [whatwg-mime](https://github.com/simongray/whatwg-mime), which read
URLs and MIME types the way browsers do, and
[webcrypto-clj](https://github.com/simongray/webcrypto-clj), which has the
bytes and their encodings.

> This library was spun out of [podcast-clj](https://github.com/simongray/podcast-clj),
> a library for making podcast software, where it fetches feeds, media
> files and the rest of what a listener subscribes to. Like podcast-clj,
> it was developed with assistance from frontier LLMs.

Getting started
---------------

The library requires Clojure 1.11+ and Java 11+. For the latest release,
add it from [Clojars](https://clojars.org/dk.simongray/wary-fetch) to the
`:deps` in your `deps.edn`:

```clojure
dk.simongray/wary-fetch {:mvn/version "0.11.0"}
```

For changes that aren't released yet, use the SHA of the latest commit on
`master` instead:

```clojure
dk.simongray/wary-fetch
{:git/url "https://github.com/simongray/wary-fetch"
 :git/sha "…"}
```

For ClojureScript, shadow-cljs only reads Git dependencies from
`deps.edn`, so also set `:deps true` in your `shadow-cljs.edn`.

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

The response also has the `:headers`, and the `:body` as text, decoded by
the charset that the server or the document declares. In ClojureScript,
`http/fetch!` gives a promise of the response.

Give the next request the `:etag` and `:last-modified` of the response,
and a document that hasn't changed comes back as a 304 without a body:

```clojure
(:status (http/fetch! "https://example.com/"
                      (select-keys response [:etag :last-modified])))
;; => 304
```

A URL on the network of the app or of its device gets no request, and
`http/fetch!` throws an ex-info with the `:type`
`:dk.simongray.wary-fetch/refused` instead:

```clojure
(http/fetch! "http://169.254.169.254/latest/meta-data")
;; throws "The request to http://169.254.169.254/latest/meta-data is not allowed"
```

The check is the `:allow-pred` option, a predicate of the URL and of each
redirect, which is `url/public?` unless you give another. To fetch from
your own network, give one of your own, e.g. `(constantly true)`.

Requests
--------

A request is a map of `:method`, `:url`, `:headers` and `:body`. Send one
with `http/send!`, or let `http/fetch!` make one for you. A request can
also have these keys:

- `:timeout-seconds`, the limit of the whole exchange, 30 by default
- `:max-bytes`, the limit of the body, 100 MB by default
- `:truncate?`, true to cut a body over `:max-bytes` rather than throw,
  which marks the response `:truncated?`
- `:max-redirects`, how many redirects to follow, 5 by default
- `:allow-pred`, a predicate of the URL and of each redirect
- `:user-agent`, the User-Agent, unless the headers name one
- `:as :bytes`, for a body of bytes, e.g. of an image, or `:as :json`,
  for the value of a JSON body, nil when it isn't JSON
- `:range`, the first and the last byte to ask for
- `:sink`, to take the body of a success in pieces, see [Media
  files](#media-files)
- `:signal`, to stop the exchange, e.g. when the user cancels: an
  AbortSignal in ClojureScript, and on the JVM an atom that turns true

An exchange that goes beyond a limit throws with the `:type`
`:dk.simongray.wary-fetch/timeout` or `:dk.simongray.wary-fetch/too-large`.
To tell whether an exception is a timeout, including one from the
platform's own client, use `http/timeout?`. One that its signal stopped
throws `:dk.simongray.wary-fetch/aborted`, which `http/aborted?` tells.

To post a form, e.g. to an OAuth token endpoint, make the request with
`http/form-request`, whose body is a hidden text, since a form often
holds a secret. `http/json-request` posts a value as JSON, and
`http/multipart-request` posts files, e.g. a photo to upload, beside the
fields of a form. The
`dk.simongray.wary-fetch.json` namespace reads and writes JSON on both
platforms, with no library. For the hashes, HMACs, random bytes, base64url
and comparisons of secrets that signatures and tokens need, use
webcrypto-clj.

A request that reaches no server throws with the `:type` `unresolved`
when the name of the host doesn't resolve, `unreachable` when the host
can't be reached and `tls` when the secure connection fails, all in the
`dk.simongray.wary-fetch` namespace. So you can tell a dead network from
a dead server, and a dead server from a bad certificate. A browser
doesn't say why a request failed, but when it knows that the device is
offline, the type is `unreachable`.

Use `http/send-public!` to send a request with `url/public?` as its
`:allow-pred`, and `http/try-send!` to get the exception in a map instead
of a throw. The `http/failure` function gives an exception as data.

The functions that make a request for you, e.g. `http/fetch!`, take the
limits, `:allow-pred` and `:user-agent` as options, and
`http/with-options` puts them on a request of your own. Unlike
`http/send!`, they take `url/public?` as the `:allow-pred` by default.
The `http/fetch!` function also takes `:credentials` and `:accept`, and
`:send` for a function that sends the request in place of `http/send!`.

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

The Authorization header that the request gets is hidden too. Use
`http/revealed` to get a request with the texts of its hidden headers,
e.g. for another HTTP client.

A token that you read from a file often ends with a line break, which
HTTP doesn't allow in a header. Trim it first, or the request throws with
the `:type` `:dk.simongray.wary-fetch/invalid-header`. The exception names
the header but not the token.

When to ask again
-----------------

A response from `http/fetch!` has the `:not-before` instant if its
Retry-After, Cache-Control or Expires header says when to ask again. To
get the same instant from the headers of any response, use
`headers/not-before`:

```clojure
(require '[dk.simongray.wary-fetch.headers :as headers])

(headers/not-before {"cache-control" "max-age=600"}
                    #inst "2026-10-10T12:00:00Z")
;; => #inst "2026-10-10T12:10:00.000-00:00"
```

The wait is as long as the server says. To poll a server that might be
set up wrong, cap the wait with `:max-wait-seconds`, which both functions
take, e.g. 3600 for an hour.

The `dk.simongray.wary-fetch.headers` namespace has functions for the
other headers of a response too, e.g. `headers/links` for a Link header.
For a Content-Type, use `mime/extract` of whatwg-mime.

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
`:sink`: a function of the bytes of each piece, an OutputStream on the
JVM, or a writable stream in Node. In ClojureScript, the function can
give a promise, and the next piece waits for it. The response then has
no `:body`, but the number of bytes the sink `:received`:

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

The limits apply to the sink too, so give `:max-bytes nil` for a file
that can be larger than 100 MB. Only the body of a success goes to the
sink, and a 404 gives its page as the `:body`, as usual.

Use `http/fetcher` to read a file in parts by byte range, and, on the
JVM, `http/digest!` to get a SHA-256 or other digest of a file as it
streams past.

The proxy
---------

A web page can only read an answer from another site when that site
allows it with CORS headers, and most don't. Make a Ring handler with
`proxy/handler`, and mount it on your own server, behind your login, at
a route such as `/proxy`:

```clojure
(require '[dk.simongray.wary-fetch.proxy :as proxy])

(def proxy-handler
  (proxy/handler))
```

The handler fetches the URL in its `url` parameter, as long as that URL
is on the public internet, and answers with what came back. In the page,
send each request through it with `proxy/sender`, and the response is
what a direct request would give:

```clojure
(http/fetch! "https://example.com/feed.xml"
             {:send (proxy/sender "/proxy")})
```

The handler's options, e.g. which headers it passes on, are in
`proxy/defaults`.

The handler holds each body in memory, so it takes 32 MB at most. For a
large file, e.g. an episode that a page saves, mount
`proxy/stream-handler` too, which passes the body on as it arrives, and
passes a Range on so that a download can resume. Its options are in
`proxy/stream-defaults`.

In ClojureScript
----------------

The functions that send give promises in ClojureScript. To write code for
Clojure and ClojureScript at once, chain them with `async/then`, which
takes a value, a future or a promise alike. The
`dk.simongray.wary-fetch.async` namespace has a few more functions like
it.

In Node, `http/send!` sends with node:http, and its guards are those of
the JVM. It looks up a name before it asks the `:allow-pred`, so that
`url/public?` checks every address of the name, and the request then
connects only to those addresses. It follows the redirects itself, and
asks the `:allow-pred` about each.

In a browser, `http/send!` sends with fetch, and two of the guards are
weaker:

- A page can't look up a name, so `url/public?` checks the name itself,
  and refuses e.g. localhost and names under .local.
- The fetch function follows redirects itself, so `:allow-pred` doesn't
  see them.

Those guards matter less in a page, since its requests come from the
user's own device rather than from your server. CORS also hides the
headers that a server doesn't expose, and `:filtered?` marks such a
response.

In a Capacitor app, CORS applies to the web view too. Send through the
native HTTP of the CapacitorHttp plugin instead, with the guards of Node
but for the lookup of names:

```clojure
(require '["@capacitor/core" :refer [CapacitorHttp]]
         '[dk.simongray.wary-fetch.capacitor :as capacitor])

(http/fetch! "https://example.com/feed.xml"
             {:send (capacitor/sender CapacitorHttp)})
```

The plugin hands each body over whole, so leave large files such as
episodes to a plugin for file transfers.

In an Electron app, the page sends through the main process, where Node
sends with all its guards. Give the main process `electron/handler` and
the page `electron/sender`, as the namespace's docstring shows. The
handler keeps its own limits, whatever a page asks, so a page can't reach
the network of the computer.

Principles
----------

- **Standards first.** The code follows RFC 9110, RFC 9111 and the other
  specs it implements, and cites their sections.
- **Plain data.** Requests and responses are maps, so you can send a
  request with any HTTP client, or with a function of your own.
- **One codebase.** Apart from the client of each platform, the library
  is written in `.cljc`, and it behaves the same everywhere, as far as
  JavaScript allows. It reads each URL and its host with whatwg-url, so a
  host that a browser would read as an IP address, e.g. 0x7f.1, is checked
  as one.

Development
-----------

```bash
clojure -X:test                                # the tests on the JVM
npm install                                    # once, for ClojureScript
clojure -M:cljs compile test                   # the tests in Node
clojure -M:cljs compile browser-test           # the tests in a browser
node test/browser-run.mjs target/browser-test  # run them in a headless Chrome
```

The public namespaces are the root, `headers`, `url`, `proxy`, `async`,
`secret`, `json`, `capacitor` and `electron`. The others are internal:
`jvm`, `node` and `browser` are the clients of each platform, `redirect`
and `limits` hold
the rules that the clients share, and `date` and `encoding` are helpers.

License
-------

The wary-fetch project is licensed under the [MIT licence](LICENSE).
