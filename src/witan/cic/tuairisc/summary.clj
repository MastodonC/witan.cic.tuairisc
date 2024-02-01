(ns witan.cic.tuairisc.summary
  (:require
   [net.cgrand.xforms :as x]
   [tick.core :as tick]
   [tech.v3.dataset.reductions :as dsr]
   [tablecloth.api :as tc]
   [clojure.math :as maths]
   [witan.cic.tuairisc.date-utils :as du]
   [witan.cic.tuairisc.by-age :as age]))

(defn quantile-for-sorted
  "Return `q`th quantile (q*100 th percentile) for a _complete_ & _sorted_
   vector of `n` values `padded-sorted-xs`, linearly interpolating where
   the quantile lies between observations.

   Using algorithm from [org.apache.commons.math3.stat.descriptive.rank percentile class](https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/org/apache/commons/math3/stat/descriptive/rank/Percentile.html). "
  [n q padded-sorted-xs]
  ;; - Algorithm re-written in terms of (zero based) index
  ;;   (rather than 1 based position) and quantile rather than percentile:
  ;; - Let n be the length of the (sorted) array (after zero padding)
  ;;   and 0 <= q <= 1 be the desired quantile.
  ;; - If n = 1 return the unique array element (regardless of the value of q);
  ;;   otherwise
  ;; - Compute the estimated quantile index idx = q * (n + 1) -1
  ;;   and the difference d between idx and floor(idx)
  ;;   (i.e. the fractional part of idx).
  ;; - If idx < 0 return the smallest element in the array.
  ;; - Else if idx >= (n-1) return the largest element in the array.
  ;; - Else let lower be the element at index floor(idx) in the sorted array
  ;;   and let upper be the next element in the array.
  ;;   Return lower + d * (upper - lower)
  (if (= 1 n)
    (first padded-sorted-xs)
    (let [idx (dec (* q (inc n)))]
      (cond
        (< idx 0) (first padded-sorted-xs)
        (<= (dec n) idx) (last padded-sorted-xs)
        :else (let [idx-floor (maths/floor idx)
                    d (- idx idx-floor)
                    lower-val (nth padded-sorted-xs idx-floor)
                    upper-val (nth padded-sorted-xs (inc idx-floor))]
                (+ lower-val (* d (- upper-val lower-val))))))))


(defn census-extractor-fn
  [analysis-range]
  (fn [row]
    (into []
          (comp
           (filter (fn [date] (tick/<= (row :episode-start) date (row :episode-end))))
           (map (fn [date]
                  (let [age (:years (du/age (:birthdate row) date))]
                    (assoc row
                           :analysis-date date
                           :age-at-analysis-date age
                           :broad-age-group-at-analysis-date (age/broad-age-group age)
                           :age-group-at-analysis-date (age/age-group age)))))
           (filter #(% :age-group-at-analysis-date)))
          analysis-range)))

(defn event-extractor-fn [k analysis-range]
  (fn [row]
    (let [start (reduce tick/min analysis-range)
          end   (reduce tick/max analysis-range)]
      (when (tick/<= start (k row) end)
        (let [age (:years (du/age (row :birthdate) (row k)))]
          (assoc row
                 :analysis-date (.withDayOfMonth (row k) 1)
                 :age-at-analysis-date age
                 :broad-age-group-at-analysis-date (age/broad-age-group age)
                 :age-group-at-analysis-date (age/age-group age)))))))

(defn summary-reducer [value-key]
  (dsr/reducer
   value-key ;; :row-count
   ;; hfr/parallel-reducer
   ;; init-fn
   (fn [] [])
   ;; rfn
   (fn [acc x] (conj acc x))
   ;; merge fn
   (fn [x x'] (into x x'))
   ;; finaliser fn
   (fn [xs]
     (let [simulation-count  100
           observations     (count xs)
           missing-0s       (vec (repeat
                                  (- simulation-count observations)
                                  0))
           padded-sorted-xs (x/into missing-0s
                                    (x/sort)
                                    xs)]
       {:min              (quantile-for-sorted simulation-count (/   0 100) padded-sorted-xs)
        :p05              (quantile-for-sorted simulation-count (/   5 100) padded-sorted-xs)
        :q1               (quantile-for-sorted simulation-count (/  25 100) padded-sorted-xs)
        :median           (quantile-for-sorted simulation-count (/  50 100) padded-sorted-xs)
        :q3               (quantile-for-sorted simulation-count (/  75 100) padded-sorted-xs)
        :p95              (quantile-for-sorted simulation-count (/  95 100) padded-sorted-xs)
        :max              (quantile-for-sorted simulation-count (/ 100 100) padded-sorted-xs)
        :mean             (double (/ (reduce + xs) simulation-count))
        :simulation-count simulation-count
        :observations     observations}))))

(defn summarise
  ([analysis-keys value-key]
   (fn [ds]
     (as-> ds $
       (dsr/group-by-column-agg
        analysis-keys
        {:summary (summary-reducer value-key)}
        $)
       (tc/separate-column $ :summary :infer identity)
       (apply (partial tc/complete $) analysis-keys)
       (tc/replace-missing $ [:min :p05 :q1 :median :q3 :p95 :max :mean :observations :simulation-count] :value 0)
       (tc/order-by $ analysis-keys))))
  ([analysis-keys] (summarise analysis-keys :row-count)))
