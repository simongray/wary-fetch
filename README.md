wary-fetch
==========

This is a Clojure and ClojureScript library for fetching URLs that
strangers give you, e.g. a feed URL that a user types in, or a link in a
document you fetched. Requests and responses are plain maps, sent with
java.net.http on the JVM and with fetch in JavaScript, with guards on
where a request goes and on what comes back.

- **Only the public internet, if you ask.** With `url/public?` as the
  `:allow-pred`, a request for localhost, the local network or a cloud
  metadata address throws instead of going out. On the JVM, every address
  that a name resolves to is checked, and so is every redirect.
- **Limits on every exchange.** A request has a time limit and a limit on
  the size of its body, counted after gzip is undone, so neither a slow
  server nor a gzip bomb can hold you up.
- **Careful redirects.** On the JVM, a redirect never goes from https to
  http, and credentials stay behind when a redirect leaves the origin.
  The response says where the URL has moved for good.
- **Conditional requests.** A request can carry the ETag and
  Last-Modified of the last response, and the Retry-After and
  Cache-Control headers tell you when to ask again.
- **Secrets that don't print.** Passwords and tokens, and the
  Authorization header made from them, print as `#<secret>`, so a logged
  request doesn't leak them.
- **A proxy for web pages.** A Ring handler and a matching sender let a
  page fetch from sites that don't allow it, through your own server.

The same code runs on the JVM, in Node and in the browser. There are no
dependencies but Clojure.

> This library was spun out of [podcast-clj](https://github.com/simongray/podcast-clj),
> a library for making podcast software, where it fetches feeds, media
> files and the rest of what a listener subscribes to. Like podcast-clj,
> it was developed with assistance from frontier LLMs.

Getting started
---------------

It requires Clojure 1.11+ and Java 11+. Add it to the `:deps` in your
`deps.edn` as a Git dependency, with the SHA of the latest commit on
`master`:

```clojure
io.github.simongray/wary-fetch {:git/sha "…"}
```

For ClojureScript, also set `:deps true` in your `shadow-cljs.edn`, since
shadow-cljs only reads Git dependencies from `deps.edn`.

Fetch a document with `http/fetch!`:

```clojure
(require '[dk.simongray.wary-fetch :as http]
         '[dk.simongray.wary-fetch.url :as url])

(def response
  (http/fetch! "https://example.com/" {:allow-pred url/public?}))

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

A URL that the `:allow-pred` refuses throws an ex-info with the `:type`
`:dk.simongray.wary-fetch/refused`, and no request goes out:

```clojure
(http/fetch! "http://169.254.169.254/latest/meta-data"
             {:allow-pred url/public?})
;; throws "The request to http://169.254.169.254/latest/meta-data is not allowed"
```

Requests
--------

A request is a map of `:method`, `:url`, `:headers` and `:body`, which
`http/send!` sends, and `http/fetch!` makes one for you. The request can
also have these keys:

- `:timeout-seconds`, the limit of the whole exchange, 30 by default
- `:max-bytes`, the limit of the body, 100 MB by default
- `:allow-pred`, a predicate of the URL and of each redirect
- `:as :bytes`, for a body of bytes, e.g. of an image
- `:range`, the first and the last byte to ask for

An exchange that goes beyond a limit throws with the `:type`
`:dk.simongray.wary-fetch/timeout` or `:dk.simongray.wary-fetch/too-large`.
Use `http/send-public!` to send a request with `url/public?` as its
`:allow-pred`, and `http/try-send!` to get an exception as data instead.

The `http/fetch!` function takes the same limits as options, along with
`:credentials`, `:user-agent`, `:accept` and `:send`, a function to send
the request in place of `http/send!`.

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

When to ask again
-----------------

A response from `http/fetch!` has the `:not-before` instant when its
Retry-After or Cache-Control headers say how long to wait. The
`http/not-before` function reads the same headers on their own:

```clojure
(http/not-before {"cache-control" "max-age=600"}
                 #inst "2026-10-10T12:00:00Z")
;; => #inst "2026-10-10T12:10:00.000-00:00"
```

The wait is at most an hour, unless the option `:max-wait-seconds` says
otherwise, since a server that's set up wrong mustn't hold back your
polling for days.

Media files
-----------

Ask about a large file without downloading it with `http/probe!`, which
sends a HEAD request and a request for the first two bytes:

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

Use `http/fetcher` to read a file in parts by byte range, and, on the
JVM, `http/digest!` to get a SHA-256 or other digest of a file as it
streams past.

The proxy
---------

A web page can only read an answer from another site when that site
allows it with CORS headers, and most don't. Run `proxy/handler` on your
own server, behind your login, as a Ring handler for a route such as
`/proxy`:

```clojure
(require '[dk.simongray.wary-fetch.proxy :as proxy])

(def proxy-handler
  (proxy/handler))
```

It fetches the URL of its `url` parameter, only on the public internet,
and answers with what came back. In the page, send each request through
it with `proxy/sender`, and the response is what a direct request would
give:

```clojure
(http/fetch! "https://example.com/feed.xml"
             {:send (proxy/sender "/proxy")})
```

The handler's options, e.g. which headers it passes on, are in
`proxy/defaults`.

In ClojureScript
----------------

The functions that send give promises in ClojureScript. To write code for
both platforms, chain them with `async/then`, which takes a value, a
future or a promise alike. The `dk.simongray.wary-fetch.async` namespace
has a few more functions like it.

Two of the guards are weaker in JavaScript:

- The library doesn't resolve names in JavaScript, so `url/public?`
  checks the name itself, and refuses e.g. localhost and names under
  .local.
- The fetch function follows redirects itself, so `:allow-pred` doesn't
  see them.

In a browser, CORS also hides the headers that a server doesn't expose,
and `:filtered?` marks such a response.

Principles
----------

- **Standards first.** The code follows RFC 9110, RFC 9111 and the other
  specs it implements, and cites their sections.
- **Plain data.** Requests and responses are maps, so any HTTP client can
  send a request, and you can send one with a function of your own.
- **One codebase.** The library is written in `.cljc`, with no
  dependencies but Clojure, and it behaves the same on both platforms
  wherever JavaScript allows it.

Development
-----------

```bash
clojure -X:test               # the tests on the JVM
npm install                   # once, for the Node tests
clojure -M:cljs compile test  # the tests in Node
```

License
-------

The wary-fetch project is licensed under the [MIT licence](LICENSE).
