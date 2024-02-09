(ns witan.cic.tuairisc.summary.event
  (:require
   [tablecloth.api :as tc]
   [tick.core :as tick]
   [tech.v3.dataset.reductions :as dsr]
   [witan.cic.tuairisc.summary :as summary]))

(defn summarise
  ([projection-episodes {:keys [event-key analysis-keys analysis-range]}]
   (let [simulation-keys [:simulation]
         unique-keys (conj [:simulation :id] event-key)
         end-to-drop (reduce tick/max analysis-range)]
     (as-> projection-episodes $
       (tc/unique-by $ unique-keys)
       (tc/map-rows $ (summary/event-extractor-fn event-key analysis-range))
       (tc/drop-missing $)
       (dsr/group-by-column-agg
        (into simulation-keys analysis-keys)
        {:row-count (dsr/row-count)}
        $)
       (tc/drop-rows $ #(= (% :analysis-date) end-to-drop))
       ((summary/summarise analysis-keys) $))))
  ([{:keys [projection-episodes] :as config}]
   (summarise projection-episodes config)))
