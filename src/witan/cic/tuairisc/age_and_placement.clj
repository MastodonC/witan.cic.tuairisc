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
                              :age-years (:years age-at-date)})))
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

(defn care-days-per-week-by-age [year-weeks]
  (as-> year-weeks $
    (tc/group-by $ [:simulation :age-years :year-week])
    (tc/aggregate $ {:days #(dfn/sum (:days %))})
    (ds-reduce/group-by-column-agg
     [:age-years :year-week]
     {:min     (ds-reduce/prob-quantile :days 0.0)
      :low-95  (ds-reduce/prob-quantile :days 0.05)
      :q1      (ds-reduce/prob-quantile :days 0.25)
      :median  (ds-reduce/prob-quantile :days 0.50)
      :q3      (ds-reduce/prob-quantile :days 0.75)
      :high-95 (ds-reduce/prob-quantile :days 0.95)
      :max     (ds-reduce/prob-quantile :days 1.0)}
     [$])
    (tc/order-by $ [:age-years :year-week])))

(def age-colors-and-shapes
  {0  {:color [31.0, 119.0, 180.0, 255.0], :shape \/, :legend-shape \\}
   1  {:color [255.0, 152.0, 150.0, 255.0], :shape \o, :legend-shape \o}
   2  {:color [214.0, 39.0, 40.0, 255.0], :shape \A, :legend-shape \V}
   3  {:color [197.0, 176.0, 213.0, 255.0], :shape \>, :legend-shape \>}
   4  {:color [148.0, 103.0, 189.0, 255.0], :shape \x, :legend-shape \x}
   5  {:color [247.0, 182.0, 210.0, 255.0], :shape \v, :legend-shape \^}
   6  {:color [227.0, 119.0, 194.0, 255.0], :shape \S, :legend-shape \S}
   7  {:color [196.0, 156.0, 148.0, 255.0], :shape \{, :legend-shape \{}
   8  {:color [140.0, 86.0, 75.0, 255.0], :shape \s, :legend-shape \s}
   9  {:color [127.0, 127.0, 127.0, 255.0], :shape \<, :legend-shape \<}
   10 {:color [219.0, 219.0, 141.0, 255.0], :shape \}, :legend-shape \}}
   11 {:color [199.0, 199.0, 199.0, 255.0], :shape \-, :legend-shape \-}
   12 {:color [188.0, 189.0, 34.0, 255.0], :shape \V, :legend-shape \A}
   13 {:color [158.0, 218.0, 229.0, 255.0], :shape \\, :legend-shape \/}
   14 {:color [23.0, 190.0, 207.0, 255.0], :shape \^, :legend-shape \v}
   15 {:color [152.0, 223.0, 138.0, 255.0], :shape \|, :legend-shape \|}
   16 {:color [255.0, 187.0, 120.0, 255.0], :shape \O, :legend-shape \O}
   17 {:color [255.0, 127.0, 14.0, 255.0], :shape \/, :legend-shape \\}
   18 {:color [174.0, 199.0, 232.0, 255.0], :shape \o, :legend-shape \o}})
