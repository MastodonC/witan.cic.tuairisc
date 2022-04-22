(ns witan.cic.tuairisc.age-and-placement
  (:require
   [com.climate.claypoole.lazy :as lazy]
   [net.cgrand.xforms :as x]
   [tablecloth.api :as tc]
   [tech.v3.dataset :as ds]
   [tech.v3.datatype.functional :as dfn]
   [tick.core :as tick]))

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
    ;; {:years years :months months}
    [years months]))

(defn care-days-per-week [{:keys [analysis-start
                                  analysis-end]}
                          projection-epsiodes]
  (-> projection-epsiodes
      (ds/row-mapcat
       (fn [row]
         (let [birthdate (:birthdate row)
               simulation (:simulation row)
               placement-type (:placement-type row)]
           (x/into []
                   (comp
                    (filter (fn [date] (tick/< analysis-start date analysis-end)))
                    (map (fn [date]
                           (let [age-at-date (age birthdate date)]
                             {:year-week (tick/with date :day-of-week 1) ;; Move everything to the Monday of the week
                              :age-years (first age-at-date)})))
                    (x/by-key identity x/count)
                    (map (fn [rec]
                           (let [k (first rec)
                                 days (second rec)]
                             (assoc k
                                    :simulation simulation
                                    :placement-type placement-type
                                    :days days)))))
                   (tick/range (:episode-start row) (:episode-end row))))))
      (tc/group-by [:simulation :placement-type :age-years :year-week])
      (tc/aggregate {:days #(dfn/sum (:days %))})))

(defn mapcat-durations
  "Turn durations of episode-start to episode end into counts of days in
  that duration. Pass in a function that will do this. The default
  function counts care days per week."
  [{:keys [analysis-start
           analysis-end
           projection-episodes
           mapcatf
           cpu-pool]
    :or {mapcatf (partial care-days-per-week {:analysis-start analysis-start :analysis-end analysis-end})
         cpu-pool (java.util.concurrent.ForkJoinPool/commonPool)}}]
  (as-> projection-episodes $
    (tc/select-columns $ [:simulation :placement-type :birthdate :episode-start :episode-end])
    (tc/select-rows $ (fn [row] (tick/< analysis-start (:episode-end row))))
    (tc/select-rows $ (fn [row] (tick/< (:episode-start row) analysis-end)))
    (tc/group-by $ [:simulation] {:result-type :as-seq})
    (lazy/upmap cpu-pool mapcatf $)))

;;(parquet/ds-seq->parquet (str out-dir "placement-year-week-counts-2.parquet") $)
