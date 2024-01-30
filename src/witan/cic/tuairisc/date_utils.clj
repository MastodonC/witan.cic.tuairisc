(ns witan.cic.tuairisc.date-utils
  (:require [tick.core :as tick]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn age [^java.time.LocalDate birthdate
           ^java.time.LocalDate target-date]
  (let [duration ^long (.until
                        birthdate
                        target-date
                        java.time.temporal.ChronoUnit/MONTHS)
        years (quot duration 12)
        months (mod duration 12)]
    {:years years :months months}))

(defn year-week [date]
  (tick/with date :day-of-week 1))  ;; Move everything to the Monday of the week

(defn overlaps-window?
  "
ws/we window-start/window-end
ps/pe period-start/period-end

The chart below shows the relationship between the periods and whether
or not this function should return true or false

                 ws        we
   false: ps pe
    true: ps       pe
    true:          ps  pe
    true:              ps    pe
   false:                    ps  pe
    true:    ps              pe  "
  [window-start window-end period-start period-end]
  (or (tick/<= period-start window-start period-end window-end)
      (tick/<= window-start period-start period-end window-end)
      (tick/<= window-start period-start window-end period-end)
      (tick/<= period-start window-start window-end period-end)))
