(ns dk.simongray.wary-fetch.secret
  "Texts that print as #<secret> (e.g. a password or a token) so that a log
  or a REPL doesn't show them.

  The reader can't read #<secret>. So a map with a hidden text that goes to
  a file as EDN fails to read back instead of coming back without its
  secret. A debugger or a browser's console still shows the text because
  it shows the fields of an object."
  #?(:clj (:import [java.io Writer])))

(def ^:no-doc shown
  "What the printer writes in the place of a hidden text."
  "#<secret>")

#?(:clj
   (deftype Hidden [text]
     Object
     (toString [_] shown)
     (equals [_ other]
       (and (instance? Hidden other)
            (= text (.-text ^Hidden other))))
     (hashCode [_] (hash text)))

   :cljs
   (deftype Hidden [text]
     Object
     (toString [_] shown)
     (toJSON [_] shown)
     IEquiv
     (-equiv [_ other]
       (and (instance? Hidden other)
            (= text (.-text other))))
     IHash
     (-hash [_] (hash text))
     IPrintWithWriter
     (-pr-writer [_ writer _] (-write writer shown))))

#?(:clj
   (defmethod print-method Hidden
     [_ ^Writer writer]
     (.write writer ^String shown)))

#?(:clj
   (defmethod print-dup Hidden
     [_ ^Writer writer]
     (.write writer ^String shown)))

(defn hidden?
  "Whether `x` is a hidden text."
  [x]
  (instance? Hidden x))

(defn hide
  "The text `s` as a value that the printer doesn't show. A hidden text
  stays as it is, and so does nil. Two hidden texts are equal when their
  texts are."
  [s]
  (if (or (nil? s) (hidden? s))
    s
    (->Hidden s)))

(defn reveal
  "The text that `x` hides. A value that isn't hidden stays as it is, so
  the function takes a plain text too."
  [x]
  (if (hidden? x)
    (.-text ^Hidden x)
    x))

(comment
  (def token
    (hide "s3cr3t"))

  ;; a map with a secret, as a log writes it
  (pr-str {:user "ann" :token token})
  (str token)

  (reveal token)
  (= token (hide "s3cr3t"))
  #_.)
