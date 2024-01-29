(ns witan.cic.tuairisc.config
  (:require [tick.core :as tick]))

(defn extract-date [c]
  (-> (get-in c [:projection-parameters :episodes-extract-date])
      (.toDate)
      (tick/date)))

(defn training-date-range [c]
  (->> (get-in c [:projection-parameters :joiners :train-date-range])
       (mapv #(-> % (.toDate) (tick/date)))))

(defn standard-analysis-period [c]
  (let [td-range (training-date-range c)]
    [(first td-range) (.plusYears (second td-range) 5)]))

(defn standard-analysis-range-weeks [c]
  (let [[start end] (standard-analysis-period c)]
    (tick/range start end (tick/new-period 1 :weeks))))

(defn standard-analysis-range-months [c]
  (let [[start end] (standard-analysis-period c)]
    (tick/range start end (tick/new-period 1 :months))))

(defn projection-name [prefix config]
  (let [[training-start training-end] (training-date-range config)]
    (format "%s %s-%s" prefix (.getYear training-start) (.getYear training-end))))
