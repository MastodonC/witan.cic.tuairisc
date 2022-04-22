(ns witan.cic.tuairisc.projection-episodes
  (:require [tablecloth.api :as tc]))

(defn ->ds [projection-episodes-csv]
  (-> projection-episodes-csv
      (tc/dataset)
      (tc/rename-columns {"Simulation"         :simulation
                          "ID"                 :id
                          "Episode"            :episode
                          ;; "Birth Year"
                          "Admission Age"      :admission-age
                          "Birthday"           :birthdate
                          "Start"              :episode-start
                          "End"                :episode-end
                          "Placement"          :placement-type
                          "Offset"             :offset
                          "Provenance"         :provenance
                          "Placement Sequence" :placement-sequence
                          "Placement Pathway"  :placement-pathway
                          "Period Start"       :period-start
                          "Period Duration"    :period-duration
                          "Period End"         :period-end
                          ;; "Period Offset"
                          ;; "Match Offset"
                          ;; "Matched ID"
                          ;; "Matched Offset"
                          })
      (tc/select-columns [:simulation :id :episode
                          :admission-age :birthdate
                          :episode-start :episode-end :placement-type :offset
                          :provenance
                          :placement-sequence :placement-pathway
                          :period-start :period-end :period-duration])))
