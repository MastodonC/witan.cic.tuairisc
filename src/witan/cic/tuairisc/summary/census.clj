(ns witan.cic.tuairisc.summary.census
  (:require
   [tablecloth.api :as tc]
   [tick.core :as tick]
   [tech.v3.dataset :as ds]
   [tech.v3.dataset.reductions :as dsr]
   [witan.cic.tuairisc.date-utils :as du]
   [witan.cic.tuairisc.summary :as summary]))

(defn summarise [projection-episodes {:keys [analysis-keys analysis-range max-batch-size]
                                      :or {max-batch-size 10}}]
  (let [data projection-episodes
        simulation-keys [:simulation]
        analysis-start (reduce tick/min analysis-range)
        analysis-end (reduce tick/max analysis-range)]
    (as-> data $
      ;; Make sure we only select rows that will produce results in the seq
      (tc/select-rows $ #(du/overlaps-window? analysis-start analysis-end (% :episode-start) (% :episode-end)))
      (ds/row-mapcat $ (summary/census-extractor-fn analysis-range) {:max-batch-size max-batch-size :result-type :as-seq})
      (dsr/group-by-column-agg
       (into simulation-keys analysis-keys)
       {:row-count (dsr/row-count)}
       $)
      ((summary/summarise analysis-keys) $))))
