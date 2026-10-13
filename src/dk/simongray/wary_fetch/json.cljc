(ns dk.simongray.wary-fetch.json
  "JSON on both platforms, without a library: a strict reader by RFC 8259
  on the JVM, and the JSON of JavaScript in ClojureScript.

  An object reads as a map with keyword keys, unless a :key-fn says
  otherwise, an array as a vector, and a number as a long when it's an
  integer that fits, or else as a double. In JavaScript every number is a
  double, so an integer past 2^53 loses its last digits there."
  #?(:cljs (:require [goog.object :as gobj]))
  #?(:clj (:import [java.time Instant ZoneOffset]
                   [java.time.format DateTimeFormatter])))

(def max-depth
  "How deep the arrays and objects of a JSON text may nest, so that a
  hostile text can't use up the stack."
  512)

#?(:clj
   (defn- malformed
     "The exception of a text that isn't JSON."
     []
     (ex-info "Not JSON" {:type ::malformed})))

#?(:clj
   (defn- skip-space
     "The index of the first character of `s` at or after `i` that isn't
     the white space of JSON."
     ^long [^String s ^long i]
     (let [n (.length s)]
       (loop [i i]
         (if (and (< i n)
                  (let [c (.charAt s i)]
                    (or (= c \space) (= c \tab) (= c \newline) (= c \return))))
           (recur (inc i))
           i)))))

#?(:clj
   (defn- expect
     "The index after the text `word` at `i` in `s`, which must be there."
     ^long [^String s ^long i ^String word]
     (if (.startsWith s word i)
       (+ i (.length word))
       (throw (malformed)))))

#?(:clj
   (defn- digits-end
     "The index after the digits of `s` from `i`, which must have one."
     ^long [^String s ^long i]
     (let [n   (.length s)
           end (loop [j i]
                 (if (and (< j n) (Character/isDigit (.charAt s j))
                          (<= (int (.charAt s j)) (int \9)))
                   (recur (inc j))
                   j))]
       (if (= end i) (throw (malformed)) end))))

;; RFC 8259, section 6
#?(:clj
   (defn- read-number
     "The number at `i` in `s` and the index after it, as a pair."
     [^String s ^long i]
     (let [n      (.length s)
           at     (fn [j] (when (< j n) (.charAt s j)))
           j      (if (= \- (at i)) (inc i) i)
           j      (if (= \0 (at j)) (inc j) (digits-end s j))
           int?   (not (#{\. \e \E} (at j)))
           j      (if (= \. (at j)) (digits-end s (inc j)) j)
           j      (if (#{\e \E} (at j))
                    (digits-end s (if (#{\+ \-} (at (inc j))) (+ j 2) (inc j)))
                    j)
           text   (subs s i j)]
       [(if int?
          (or (parse-long text) (bigint text))
          (Double/parseDouble text))
        j])))

;; RFC 8259, section 7
#?(:clj
   (defn- read-string*
     "The string whose opening quote is at `i` in `s` and the index after
     its closing quote, as a pair."
     [^String s ^long i]
     (let [n  (.length s)
           sb (StringBuilder.)]
       (loop [j (inc i)]
         (when (>= j n)
           (throw (malformed)))
         (let [c (.charAt s j)]
           (cond
             (= c \") [(str sb) (inc j)]

             (< (int c) 0x20) (throw (malformed))

             (= c \\)
             (let [e (if (< (inc j) n) (.charAt s (inc j)) (throw (malformed)))]
               (if (= e \u)
                 (let [hex (when (<= (+ j 6) n) (subs s (+ j 2) (+ j 6)))]
                   (if (and hex (re-matches #"[0-9a-fA-F]{4}" hex))
                     (do (.append sb (char (Integer/parseInt hex 16)))
                         (recur (+ j 6)))
                     (throw (malformed))))
                 (do (.append sb (case e
                                   \" \"
                                   \\ \\
                                   \/ \/
                                   \b \backspace
                                   \f \formfeed
                                   \n \newline
                                   \r \return
                                   \t \tab
                                   (throw (malformed))))
                     (recur (+ j 2)))))

             :else
             (do (.append sb c)
                 (recur (inc j)))))))))

#?(:clj
   (declare read-value))

#?(:clj
   (defn- read-items
     "The elements of the array or the members of the object that opens at
     `i` in `s`, up to `close`, the closing character, by `item`, a function
     of the index that gives what an item adds and the index after it, as a
     pair of the items and the index after `close`."
     [^String s ^long i close item]
     (let [start (skip-space s (inc i))]
       (if (and (< start (.length s)) (= close (.charAt s start)))
         [[] (inc start)]
         (loop [j start acc (transient [])]
           (let [[x j] (item j)
                 j     (skip-space s j)
                 c     (when (< j (.length s)) (.charAt s j))]
             (cond
               (= c \,)    (recur (skip-space s (inc j)) (conj! acc x))
               (= c close) [(persistent! (conj! acc x)) (inc j)]
               :else       (throw (malformed)))))))))

#?(:clj
   (defn- read-value
     "The value at `i` in `s`, which nests at `depth`, and the index after
     it, as a pair, with object keys by `key-fn`."
     [^String s ^long i depth key-fn]
     (let [i (skip-space s i)
           c (if (< i (.length s)) (.charAt s i) (throw (malformed)))]
       (case c
         \{ (do (when (> depth max-depth) (throw (malformed)))
                (let [member (fn [j]
                               (when-not (and (< j (.length s))
                                              (= \" (.charAt s j)))
                                 (throw (malformed)))
                               (let [[k j] (read-string* s j)
                                     j     (skip-space s j)
                                     j     (expect s j ":")
                                     [v j] (read-value s j (inc depth) key-fn)]
                                 [[(key-fn k) v] j]))
                      [kvs j] (read-items s i \} member)]
                  [(into {} kvs) j]))
         \[ (do (when (> depth max-depth) (throw (malformed)))
                (read-items s i \] #(read-value s % (inc depth) key-fn)))
         \" (read-string* s i)
         \t [true (expect s i "true")]
         \f [false (expect s i "false")]
         \n [nil (expect s i "null")]
         (read-number s i)))))

#?(:cljs
   (defn- from-js
     "The value that JSON.parse gave, `x`, which nests at `depth`, as
     Clojure data, with object keys by `key-fn`. It throws when it nests
     deeper than max-depth, as the reader of the JVM does."
     [x depth key-fn]
     (let [nested? (or (array? x)
                       (and (some? x) (= "object" (goog/typeOf x))))]
       (when (and nested? (> depth max-depth))
         (throw (ex-info "Not JSON" {:type ::malformed})))
       (cond
         (array? x)
         (mapv #(from-js % (inc depth) key-fn) x)

         nested?
         (into {}
               (map (fn [k] [(key-fn k) (from-js (gobj/get x k) (inc depth)
                                                 key-fn)]))
               (js-keys x))

         :else
         x))))

(defn read-json
  "The value of the JSON text `s`, read by the `opts` below, or nil when `s`
  isn't JSON or nests deeper than max-depth.

  The `opts` are :key-fn, a function of the name of each member of an
  object that gives its key, keyword by default."
  ([s]
   (read-json s {}))
  ([s {:keys [key-fn] :or {key-fn keyword} :as opts}]
   {:pre [(map? opts)]}
   (when (string? s)
     (try
       #?(:clj  (let [[x i] (read-value s 0 1 key-fn)]
                  (when (= (.length ^String s) (skip-space s i))
                    x))
          :cljs (from-js (js/JSON.parse s) 1 key-fn))
       (catch #?(:clj Exception :cljs :default) _
         nil)
       #?(:clj (catch StackOverflowError _
                 nil))))))

#?(:clj
   (defn- write-string!
     "The StringBuilder `sb` with the string `s` as JSON, escaped as RFC 8259
     says."
     [^StringBuilder sb ^String s]
     (.append sb \")
     (dotimes [i (.length s)]
       (let [c (.charAt s i)]
         (case c
           \" (.append sb "\\\"")
           \\ (.append sb "\\\\")
           \newline (.append sb "\\n")
           \return (.append sb "\\r")
           \tab (.append sb "\\t")
           (if (< (int c) 0x20)
             (.append sb (format "\\u%04x" (int c)))
             (.append sb c)))))
     (.append sb \")))

#?(:clj
   (def ^:no-doc instant-format
     "The form of an instant in UTC that JavaScript's JSON gives it."
     (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")))

(defn- key-name
  "The name of the member of an object that the key `k` gives."
  [k]
  (if (keyword? k) (subs (str k) 1) (str k)))

#?(:clj
   (defn- write!
     "The StringBuilder `sb` with the value `x` as JSON."
     [^StringBuilder sb x]
     (cond
       (nil? x)     (.append sb "null")
       (boolean? x) (.append sb (str x))
       (string? x)  (write-string! sb x)
       (keyword? x) (write-string! sb (key-name x))

       (number? x)
       (if (and (float? x) (or (Double/isNaN x) (Double/isInfinite x)))
         (.append sb "null")
         (.append sb (str (if (ratio? x) (double x) x))))

       (map? x)
       (do (.append sb \{)
           (reduce (fn [first? [k v]]
                     (when-not first? (.append sb \,))
                     (write-string! sb (key-name k))
                     (.append sb \:)
                     (write! sb v)
                     false)
                   true
                   x)
           (.append sb \}))

       (coll? x)
       (do (.append sb \[)
           (reduce (fn [first? v]
                     (when-not first? (.append sb \,))
                     (write! sb v)
                     false)
                   true
                   x)
           (.append sb \]))

       ;; after maps and collections, since inst? is a satisfies? check,
       ;; which takes microseconds for a type that doesn't satisfy it
       (inst? x)
       (write-string! sb (.format ^DateTimeFormatter instant-format
                                  (.atOffset (Instant/ofEpochMilli (inst-ms x))
                                             ZoneOffset/UTC)))

       :else
       (write-string! sb (str x)))))

(defn write-json
  "The JSON text of the value `x`: a map as an object, with keywords as
  their names, namespace and all, a vector, a list or a set as an array,
  an instant as its date and time in UTC, and a number that isn't finite
  as null, as JavaScript writes it."
  [x]
  #?(:clj  (str (write! (StringBuilder.) x))
     :cljs (js/JSON.stringify (clj->js x :keyword-fn key-name))))

(comment
  (read-json "{\"me\":\"https://simon.example/\",\"scope\":[\"create\"]}")
  ;; => {:me "https://simon.example/" :scope ["create"]}
  (write-json {:type ["h-entry"] :properties {:content ["Hello"]}})
  #_.)
