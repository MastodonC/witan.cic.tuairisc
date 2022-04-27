(ns witan.cic.tuairisc.report
  (:require
   [clojure.string :as s]
   [kixi.large :as xl]
   [kixi.plot :as plot]
   [kixi.plot.series :as series]
   [tablecloth.api :as tc]))

(defn friendly-column-names [ds]
  (tc/rename-columns ds
                     {:placement-type "Placement Type"
                      :age-years "Age"
                      :year-week "Week Commencing"
                      :min "min"
                      :low-95 "Low 95%"
                      :q1 "q1"
                      :median "Median/Actual"
                      :q3 "q3"
                      :high-95 "High 95%"
                      :max "Max"}))

(defn format-table [ds domain-key]
  (-> ds
      (tc/reorder-columns [domain-key :year-week
                           :min :low-95 :q1
                           :median
                           :q3 :high-95 :max])
      friendly-column-names))

(defn summarise-simulations
  "This expects the output of care-days-by-keys"
  [{:keys [care-days-data domain-key time-key]
    :or {domain-key :age-years
         time-key :year-weeks}
    :as m}]
  (assoc m
         :summary
         (-> care-days-data
             (tc/order-by [time-key])
             (tc/group-by [domain-key] {:result-type :as-map})
             (update-vals (fn [ds] {:data ds}))
             (update-keys (fn [k] {:domain-key (key (first k)) :domain-value (val (first k))})))))

(defn summary-series [{:keys [summary
                              color-and-shape-map ;; colors-and-shapes
                              time-key]
                       :as m}]
  (assoc m :summary
         (reduce-kv (fn [m k v] ;; FIXME: factor this out into a function that could be passed in, perhaps at the level below the assoc onto the accumulator
                      (try
                        (let [{:keys [domain-value]} k
                              {:keys [data]} v
                              color (-> domain-value color-and-shape-map :color)
                              shape (-> domain-value color-and-shape-map :shape)
                              line-y :median
                              ribbon-1-high-y :q3 ribbon-1-low-y :q1
                              ribbon-2-high-y :high-95 ribbon-2-low-y :low-95]
                          (assoc m k (assoc v
                                            :series
                                            (series/ds->line-and-double-ribbon
                                             data
                                             {:color color :shape shape
                                              :x time-key :line-y line-y
                                              :stroke 1.0 :size 10.0
                                              :ribbon-1-high-y ribbon-1-high-y :ribbon-1-low-y ribbon-1-low-y
                                              :ribbon-2-high-y ribbon-2-high-y :ribbon-2-low-y ribbon-2-low-y}))))
                        (catch Exception e
                          (throw (ex-info (format "Failed to create series for %s" (:domain-value k)) {:k k :v v} e)))))
                    {}
                    summary)))

(defn summary-legend [{:keys [summary
                              color-and-shape-map ;; colors-and-shapes
                              ]
                       :as m}]
  (assoc m :summary
         (reduce-kv (fn [m k v]
                      (let [{:keys [domain-value]} k
                            color (-> domain-value color-and-shape-map :color)
                            shape (-> domain-value color-and-shape-map :legend-shape)]
                        (assoc m k (assoc v
                                          :legend
                                          (series/legend-spec domain-value color shape)))))
                    {}
                    summary)))

;; This gets mapped over the chart selectors
(defn summary-chart [{:keys [summary ;; this has all the data and series
                             domain-key]
                      :as summary-items}
                     {:keys [chart-title chart-selector ;; these two are based on what is currently in main-report but done up with keywords and then merged with the rest
                             ;; the above lets me do stuff like have different bottom/top for different charts for comparison
                             ;; bottom top ; These should be partialed in with the chartf
                             chartf
                             top
                             bottom
                             watermark
                             legend-label
                             base-chart-spec]
                      :as chart-configuration}]
  (let [summary-subset (into (sorted-map-by
                              (fn [key1 key2]
                                (compare (:domain-value key1)
                                         (:domain-value key2))))
                             (select-keys
                              summary
                              (into []
                                    (map (fn [s]
                                           {:domain-key domain-key :domain-value s}))
                                    chart-selector)))
        series (into []
                     (comp
                      (map (fn [[k v]]
                             (:series v)))
                      cat)
                     summary-subset)
        legend (into []
                     (map (fn [[k v]]
                            (:legend v)))
                     summary-subset)
        data (apply tc/concat-copying
                    (into []
                          (map (fn [[k v]]
                                 (:data v)))
                          summary-subset))]
    (-> summary-items
        (merge chart-configuration)
        (assoc :title chart-title) ;; should be sheet title?
        (assoc :data data)
        (assoc :chart
               (-> {::series/series series
                    ::series/legend-spec legend}
                   ((fn [c]
                      (if top
                        (assoc c ::plot/top top)
                        c)))
                   ((fn [c]
                      (if bottom
                        (assoc c ::plot/bottom bottom)
                        c)))
                   (merge {::plot/legend-label legend-label
                           ::plot/title {::plot/label chart-title}}
                          base-chart-spec)
                   (plot/add-overview-legend-items)
                   chartf
                   (update ::plot/canvas plot/add-watermark watermark))))))

(defn sanitise-tab-name [tab-name]
  (-> tab-name
      (s/replace "/" " ")))

(defn ->excel
  "A sheet-spec should have a:
   - title
   - chart
   - data
   - optional display-table

  If there is no format-table-f to be applied to all data in the specs
  to create display tables then the values in display-table will be
  used instead."
  [sheet-specs
   {:keys [format-table-f
           file-name]}]
  (-> (into []
            (map (fn [{:keys [title chart data display-table]
                      :as _config}]
                   {::xl/sheet-name (sanitise-tab-name title)
                    ::xl/images [{::xl/image (-> chart ::plot/canvas :buffer plot/->byte-array)}]
                    ::xl/data (if format-table-f
                                (format-table-f data)
                                display-table)}))
            sheet-specs)
      (xl/create-workbook)
      (xl/save-workbook! file-name)))
