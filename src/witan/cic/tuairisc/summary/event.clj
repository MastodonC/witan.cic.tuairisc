(ns witan.cic.tuairisc.summary.event
  (:require
   [tablecloth.api :as tc]
   [witan.cic.tuairisc.summary :as summary]))

(defn event-summary [projection-episodes {:keys [event-key analysis-keys analysis-range]}]
  (let [data projection-episodes
        simulation-keys [:simulation]
        unique-keys (conj [:simulation :id] event-key)]
    (as-> data $
      (tc/unique-by $ unique-keys)
      (tc/map-rows $ (summary/event-extractor-fn event-key analysis-range))
      (tc/drop-missing $)
      (tc/group-by $ (into simulation-keys analysis-keys))
      (tc/aggregate $ {:row-count tc/row-count})
      ((summary/summarise analysis-keys) $))))
