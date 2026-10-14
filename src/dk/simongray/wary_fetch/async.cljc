(ns dk.simongray.wary-fetch.async
  "Futures on the JVM and promises in ClojureScript behind one set of
  functions, so that you write code that waits for answers only once. The
  functions take a value, a future or a promise alike."
  (:refer-clojure :exclude [iteration some])
  (:require [clojure.string :as str]))

(defn then
  "The result of `f` on the value of `x` (in ClojureScript a promise of
  it). The `x` is a value, a future on the JVM or a promise in
  ClojureScript."
  [x f]
  #?(:clj  (f (if (future? x) @x x))
     :cljs (.then (js/Promise.resolve x) f)))

(declare attempt)

(defn all
  "The values of `xs` (each a value, a future or a promise) as a vector,
  or the results of `f` on each of them (in ClojureScript a promise of
  it). On the JVM it returns once every future has its value, and the
  futures run at once.

  With `f`, every call is under way before it waits for any, e.g.
  (all force [(delay (a!)) (delay (b!))]) runs a! and b! at once."
  ([xs]
   #?(:clj  (mapv #(if (future? %) @% %) (vec xs))
      :cljs (.then (js/Promise.all (into-array xs)) vec)))
  ([f xs]
   ;; on the JVM, each call runs on a future of its own, since a call that
   ;; chains its own futures waits for them
   #?(:clj  (let [unwrap (fn [{:keys [value error]}]
                           (if error (throw error) value))]
              (->> (all (map (fn [x] (attempt #(f x))) xs))
                   (map unwrap)
                   all))
      :cljs (all (map f xs)))))

(defn in-groups
  "The results of `f` on each of `xs` as a vector, with `n` calls at a time
  (in ClojureScript a promise of it)."
  [f n xs]
  (reduce (fn [acc group]
            (then acc (fn [done] (then (all f group) #(into done %)))))
          []
          (partition-all n xs)))

(defn some
  "The first logical true result of `f` on the `xs`, or nil, as
  clojure.core/some returns it (in ClojureScript a promise of it).

  Each call of `f` returns a value, a future or a promise. The calls go
  one at a time, so it makes none after the first true result."
  [f xs]
  (reduce (fn [found x] (then found #(or % (f x))))
          ;; a promise in ClojureScript, even when there are no xs
          (then nil identity)
          xs))

(defn attempt
  "The result of calling `f` as a map of its :value, in a future on the
  JVM and a promise in ClojureScript. When `f` throws (or in ClojureScript
  rejects), the map has the :error instead.

  On the JVM each attempt runs on a future of its own, so several run at
  once. Errors count too, e.g. a StackOverflowError from deeply nested
  input, as anything thrown does in ClojureScript."
  [f]
  #?(:clj  (future (try {:value (f)} (catch Throwable e {:error e})))
     :cljs (-> (js/Promise.resolve nil)
               (.then (fn [_] (f)))
               (.then (fn [v] {:value v}))
               (.catch (fn [e] {:error e})))))

(defn iteration
  "The values that clojure.core/iteration returns for `step!` and `opts`,
  as a vector of at most :limit values (in ClojureScript a promise of it).

  The `opts` are the :somef, :vf, :kf and :initk of
  clojure.core/iteration, and :limit. The `step!` function returns a
  value, a future or a promise, so the same code pages through an API on
  both platforms, as in the example of clojure.core/iteration:

      (iteration get-page! :initk \"a\" :vf :items :kf :next-page :limit 10)"
  [step! & {:keys [somef vf kf initk limit]
            :or   {somef some? vf identity kf identity limit ##Inf}
            :as   opts}]
  #?(:clj  (let [step (fn [k]
                        (let [x (step! k)]
                          (if (future? x) @x x)))]
             (into []
                   (take limit)
                   (clojure.core/iteration step
                                           :somef somef
                                           :vf    vf
                                           :kf    kf
                                           :initk initk)))
     :cljs (letfn [(more [values ret]
                     (if (or (not (somef ret))
                             (>= (count values) limit))
                       values
                       (let [values (conj values (vf ret))
                             k      (when (< (count values) limit)
                                      (kf ret))]
                         (if (some? k)
                           (.then (js/Promise.resolve (step! k))
                                  #(more values %))
                           values))))]
             (.then (js/Promise.resolve (step! initk)) #(more [] %)))))

(defn explain
  "The message of the failure `e`, with the messages of its causes that
  add to it. The `e` is an exception, or another value that was thrown.

  The JDK's client and Node's fetch put the reason that a connection
  failed in a cause, e.g. a java.net.ConnectException with the cause
  java.nio.channels.UnresolvedAddressException."
  [e]
  (->> (iterate #?(:clj ex-cause :cljs #(some-> % .-cause)) e)
       (take-while some?)
       (take 4)
       (map #(or (ex-message %) (str %)))
       dedupe
       (str/join ": ")))

(comment
  ;; a value, and a deferred value
  (then 1 inc)
  (then (attempt #(/ 6 3)) :value)
  (all [1 (attempt #(throw (ex-info "down" {})))])

  ;; an API that answers in pages, read to its end
  (iteration {nil {:items [1 2] :next "b"} "b" {:items [3]}}
             :vf :items
             :kf :next)

  (explain (ex-info "The request failed" {} (ex-info "Connection refused" {})))
  #_.)
