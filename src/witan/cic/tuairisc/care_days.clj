(ns witan.cic.tuairisc.care-days
  (:require
   [com.climate.claypoole.lazy :as lazy]
   [net.cgrand.xforms :as x]
   [tablecloth.api :as tc]
   [tech.v3.dataset :as ds]
   [tech.v3.libs.parquet :as parquet]
   [tech.v3.dataset.reductions :as ds-reduce]
   [tech.v3.datatype.functional :as dfn]
   [tick.core :as tick]
   [witan.cic.tuairisc.date-utils :as du]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn care-days-per-week
  "This gives sums per age and placement per simulation that can be
  rolled up and then further analysed."
  [{:keys [analysis-start
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
                              :age-years (:years age-at-date)})))
                    (x/by-key identity x/count)
                    (map (fn [rec]
                           (let [k (first rec)
                                 days (second rec)]
                             (assoc k
                                    :simulation simulation
                                    :placement-type placement-type
                                    :days days)))))
                   (tick/range (:episode-start row) (:episode-end row))))))))

(defn mapcat-durations
  "Turn durations of episode-start to episode end into counts of days in
  that duration. Pass in a function that will do this. The default
  function counts care days per week.

  Takes a map like:

  {:projection-episodes (tpe/->ds projection-episodes-csv)
   :cpu-pool            cpu-pool
   :analysis-start      (tick/new-date 2018 3 31)
   :analysis-end        (tick/new-date 2026 4 1)
   :group-keys [:simulation :placement-type :age-years :year-week]}

  It returns a lazy, unordered sequence of simulations summed by the
  group-keys which default to:
   [:simulation :placement-type :age-years :year-week]"
  [{:keys [analysis-start
           analysis-end
           projection-episodes
           mapcatf
           group-keys
           cpu-pool]
    :or {mapcatf (partial care-days-per-week {:analysis-start analysis-start :analysis-end analysis-end})
         group-keys [:simulation :placement-type :age-years :year-week]
         cpu-pool (java.util.concurrent.ForkJoinPool/commonPool)}}]
  (as-> projection-episodes $
    (tc/select-columns $ [:simulation :placement-type :birthdate :episode-start :episode-end])
    (tc/select-rows $ (fn [row] (tick/< analysis-start (:episode-end row))))
    (tc/select-rows $ (fn [row] (tick/< (:episode-start row) analysis-end)))
    (tc/group-by $ [:simulation] {:result-type :as-seq})
    (lazy/upmap cpu-pool
                (fn [ds]
                  (-> ds
                      mapcatf
                      (tc/group-by group-keys)
                      (tc/aggregate {:days #(dfn/sum (:days %))})))
                $)))

(defn durations->parquet
  "This expects a seq of duration data produced from something like mapcat-durations."
  [duration-data out-file]
  (parquet/ds-seq->parquet out-file duration-data))

(defn parquet->durations [in-file]
  (parquet/parquet->ds-seq in-file {:key-fn keyword}))

(defn care-days-by-keys [{:keys [domain-key time-key value-key]
                          :or {value-key :days}}
                         simulation-sums]
  (as-> simulation-sums $
    (tc/group-by $ [:simulation domain-key time-key])
    (tc/aggregate $ {value-key #(dfn/sum (value-key %))})
    (ds-reduce/group-by-column-agg
     [domain-key time-key]
     {:min     (ds-reduce/prob-quantile value-key 0.0)
      :low-95  (ds-reduce/prob-quantile value-key 0.05)
      :q1      (ds-reduce/prob-quantile value-key 0.25)
      :median  (ds-reduce/prob-quantile value-key 0.50)
      :q3      (ds-reduce/prob-quantile value-key 0.75)
      :high-95 (ds-reduce/prob-quantile value-key 0.95)
      :max     (ds-reduce/prob-quantile value-key 1.0)}
     [$])
    (tc/order-by $ [domain-key time-key])))
