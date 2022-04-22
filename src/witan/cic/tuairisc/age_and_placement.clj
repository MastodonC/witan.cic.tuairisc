(ns witan.cic.tuairisc.age-and-placement
  (:require
   [com.climate.claypoole.lazy :as lazy]
   [net.cgrand.xforms :as x]
   [tablecloth.api :as tc]
   [tech.v3.dataset :as ds]
   [tech.v3.dataset.reductions :as ds-reduce]
   [tech.v3.datatype.functional :as dfn]
   [tick.core :as tick]
   [witan.cic.tuairisc.date-utils :as du]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

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
                           (let [age-at-date (du/age birthdate date)]
                             {:year-week (du/year-week date)
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
