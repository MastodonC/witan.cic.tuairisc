(ns witan.cic.tuairisc.summary.census
  (:require
   [ham-fisted.api :as hf]
   [ham-fisted.reduce :as hfr]
   [tablecloth.api :as tc]
   [tick.core :as tick]
   [tech.v3.dataset :as ds]
   [tech.v3.dataset.reductions :as dsr]
   [witan.cic.tuairisc.date-utils :as du]
   [witan.cic.tuairisc.summary :as summary]))

;; ds/row-mapcat is too slow
(defn range->census [projection-episodes {:keys [analysis-range max-batch-size]
                                          :or {max-batch-size 10}}]
  (let [analysis-start (reduce tick/min analysis-range)
        analysis-end (reduce tick/max analysis-range)]
    (as-> projection-episodes $
      (tc/select-rows $ #(du/overlaps-window? analysis-start analysis-end (% :episode-start) (% :episode-end)))
      (ds/row-mapcat $ (summary/census-extractor-fn analysis-range) {:max-batch-size max-batch-size :result-type :as-seq}))))

;; I think this version should be faster than the ds/row-mapcat
;; version, but it seems to fail after running a few times and I
;; haven’t been able to figure out why. I’m leaving it here as I’d
;; like to move over to this code eventually
#_
(defn range->census [projection-episodes {:keys [analysis-range max-batch-size]
                                          :or {max-batch-size 10}}]
  (let [analysis-start (reduce tick/min analysis-range)
        analysis-end (reduce tick/max analysis-range)]
    (as-> projection-episodes $
      (tc/select-rows $ #(du/overlaps-window? analysis-start analysis-end (% :episode-start) (% :episode-end)))
      (tc/rows $ :as-maps)
      (hfr/preduce
       ;; init-val
       (fn [] (hf/mut-list))
       ;; rfn
       (fn [acc x] (let [ds (tc/dataset ((summary/census-extractor-fn analysis-range) x))]
                     (if (zero? (tc/row-count ds))
                       acc
                       (hf/conj! acc ds))))
       ;; merge-fn
       (fn [x x'] (hf/add-all! x x'))
       $)
      (hf/immut-list $))))


(defn summarise-simulation-census [data {:keys [analysis-keys]}]
  (let [simulation-keys [:simulation]]
    (dsr/group-by-column-agg
     (into simulation-keys analysis-keys)
     {:row-count (dsr/row-count)}
     data)))

(defn summarise [projection-episodes {:keys [analysis-keys]
                                      :as config}]
  (let [data projection-episodes]
    (as-> data $
      ;; Make sure we only select rows that will produce results in the seq
      (range->census $ config)
      (summarise-simulation-census $ config)
      ((summary/summarise analysis-keys) $))))
