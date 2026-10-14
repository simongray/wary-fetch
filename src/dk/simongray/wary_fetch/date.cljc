(ns ^:no-doc dk.simongray.wary-fetch.date
  "The dates of HTTP headers, by RFC 9110, section 5.6.7: the IMF-fixdate,
  and the obsolete forms of RFC 850 and asctime, which a recipient has to
  parse too.

  Servers sometimes write them a little wrong, so the parser also accepts
  a missing weekday, a day or an hour of one digit, and a zone of UTC or
  an offset. A zone that isn't known reads as UTC."
  (:require [clojure.string :as str]))

(def months
  "The months by their names of three letters, in lower case."
  {"jan" 1 "feb" 2 "mar" 3 "apr" 4 "may" 5 "jun" 6
   "jul" 7 "aug" 8 "sep" 9 "oct" 10 "nov" 11 "dec" 12})

(def imf-date
  "The pattern of an IMF-fixdate or a date of RFC 850, with the day, the
  month, the year, the hours, the minutes, the seconds and the zone as its
  groups."
  #"\s*(?:[A-Za-z]+,\s*)?(\d{1,2})[ -]([A-Za-z]{3})[ -](\d{4}|\d{2}) (\d{1,2}):(\d{2}):(\d{2})(?: +([A-Za-z]+|[+-]\d{4}))?\s*")

(def asctime-date
  "The pattern of a date in the asctime form, with the month, the day, the
  hours, the minutes, the seconds and the year as its groups."
  #"\s*[A-Za-z]{3} ([A-Za-z]{3}) +(\d{1,2}) (\d{1,2}):(\d{2}):(\d{2}) (\d{4})\s*")

(defn- parts
  "The day, the month, the year, the hours, the minutes, the seconds and
  the zone of the date `s`, as strings, or nil when it isn't a date."
  [s]
  (if-let [[_ & parts] (re-matches imf-date s)]
    (vec parts)
    (when-let [[_ month day hours minutes seconds year]
               (re-matches asctime-date s)]
      [day month year hours minutes seconds nil])))

(defn- this-year
  "The year now, in UTC."
  []
  #?(:clj  (.getYear (java.time.LocalDate/now java.time.ZoneOffset/UTC))
     :cljs (.getUTCFullYear (js/Date.))))

(defn- full-year
  "The year `y` with four digits. Two digits make the year no more than 50
  years after this one, as RFC 9110 asks for a date of RFC 850."
  [y]
  (if (< y 100)
    (let [now  (this-year)
          year (+ (- now (mod now 100)) y)]
      (if (> year (+ now 50))
        (- year 100)
        year))
    y))

(defn- leap?
  "Whether the year `y` has a 29 February."
  [y]
  (and (zero? (mod y 4))
       (or (pos? (mod y 100))
           (zero? (mod y 400)))))

(defn- days-in
  "The number of days of the month `m` of the year `y`."
  [y m]
  (case (long m)
    2         (if (leap? y) 29 28)
    (4 6 9 11) 30
    31))

;; the days since 1970-01-01 of a date of the proleptic Gregorian calendar,
;; by the days_from_civil of Howard Hinnant
(defn- epoch-day
  "The number of days from 1 January 1970 to the day `d` of the month `m`
  of the year `y`."
  [y m d]
  (let [y   (if (<= m 2) (dec y) y)
        era (quot (if (neg? y) (- y 399) y) 400)
        yoe (- y (* era 400))
        doy (+ (quot (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn- offset-ms
  "The offset from UTC of the `zone` in milliseconds, which is 0 for GMT,
  UTC, any other name and none."
  [zone]
  (if-let [[_ sign hours minutes]
           (some->> zone (re-matches #"([+-])(\d{2})(\d{2})"))]
    (* (if (= "-" sign) -1 1)
       (+ (* 60 (parse-long hours)) (parse-long minutes))
       60000)
    0))

(defn epoch-ms
  "The date of a header `s` in epoch milliseconds, or nil when it isn't a
  date or names a day or a time that doesn't exist."
  [s]
  (when-let [[day month year hours minutes seconds zone] (some-> s parts)]
    (let [m                           (months (str/lower-case month))
          [d y hours minutes seconds] (map parse-long
                                           [day year hours minutes seconds])
          y                           (full-year y)]
      (when (and m
                 (<= 1 d)
                 (<= d (days-in y m))
                 (< hours 24)
                 (< minutes 60)
                 (< seconds 61))
        (- (+ (* 86400000 (epoch-day y m d))
              (* 1000 (+ (* 3600 hours) (* 60 minutes) seconds)))
           (offset-ms zone))))))
