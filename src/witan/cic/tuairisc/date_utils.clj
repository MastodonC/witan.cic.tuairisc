(ns witan.cic.tuairisc.date-utils
  (:require [tick.core :as tick]))

(defn age [^java.time.LocalDate birthdate
           ^java.time.LocalDate target-date]
  (let [duration ^long (.until
                        birthdate
                        target-date
                        java.time.temporal.ChronoUnit/MONTHS)
        years (quot duration 12)
        months (mod duration 12)]
    ;; {:years years :months months}
    [years months]))

(defn year-week [date]
  (tick/with date :day-of-week 1))  ;; Move everything to the Monday of the week
