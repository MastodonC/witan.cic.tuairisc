(ns witan.cic.tuairisc.charting
  (:require
   [clojure.string :as str]
   [tablecloth.api :as tc]))

(def full-height 800)
(def full-width 1200)

(def colors
  [;; tableau 10 without red
   "#4e79a7"
   "#f28e2b"
   #_"#e15759"
   "#59a14f"
   "#d36295"
   "#9c755f"
   "#edc948"
   "#79706e"
   "#b07aa1"
   "#76b7b2"
   ;; tableau 20 lighter shades
   "#a0cbe8"
   "#ffbe7d"
   "#8cd17d"
   "#b6992d"
   "#f1ce63"
   "#499894"
   "#86bcb6"
   "#bab0ac"
   "#ff9da7"
   "#fabfd2"
   "#d4a6c8"
   "#d7b5a6"])

(defn color-and-shape-lookup [domain]
  (tc/dataset
   {:domain-value domain
    :color (cycle colors)
    :shape (cycle ["circle" ;; ○
                   "square" ;; □
                   "triangle-up" ;; △
                   "triangle-down" ;; ▽
                   "triangle-right" ;; ▷
                   "triangle-left" ;; ◁
                   "cross" ;; +
                   "diamond" ;; ◇
                   ])
    :unicode-shape (cycle ["○"
                           "□"
                           "△"
                           "▽"
                           "▷"
                           "◁"
                           "+"
                           "◇"])}
   #_{:domain-value domain
      :color (cycle ["#29733c" "#fa814c" "#256cc6" "#fbe44c" "#50b938" "#59c4b8"])
      :shape (cycle ["circle", "square", "cross", "diamond", "triangle-up", "triangle-down", "triangle-right", "triangle-left"])}))


(defn color-map [plot-data color-field color-lookup]
  (let [group-keys (into (sorted-set) (get plot-data color-field))
        filtered-colors (tc/select-rows color-lookup #(group-keys (get % :domain-value)))]
    {:field color-field
     :scale {:range (into [] (:color filtered-colors))
             :domain (into [] (:domain-value filtered-colors))}}))


(defn shape-map [plot-data shape-field shape-lookup]
  (let [group-keys (into (sorted-set) (get plot-data shape-field))
        filtered-shapes (tc/select-rows shape-lookup #(group-keys (get % :domain-value)))]
    {:field shape-field
     :scale {:range (into [] (:shape filtered-shapes))
             :domain (into [] (:domain-value filtered-shapes))}}))

;;; # tooltip helpers
(defn five-number-summary-string
  "Format five number summaries [`orl` `irl` `y` `iru` `oru`] into a single string
   of the form \"y (orl (irl↔iru) oru)\"
   using format `fmt` for each summary (default \"%,.0f\")
   after applying (optional) function `f` to it.
   (So for percentages specify `:fmt \"%.0f%%\" :f (partial * 100)`.)"
  ([xs] (five-number-summary-string {} xs))
  ([{:keys [fmt f] :or {fmt "%,.0f", f identity}} [orl irl y iru oru]]
   (apply format
          (apply format "%s (%s (%s↔%s) %s)" (repeat 5 fmt))
          (map f [y orl irl iru oru]))))

(defn five-number-summary-tooltip
  [& {:keys [tooltip-field
             orl irl y iru oru]
      :or {tooltip-field :tooltip-column}
      :as cfg}]
  (fn [ds]
    (-> ds
        (tc/map-columns tooltip-field [orl irl y iru oru]
                        #(five-number-summary-string (select-keys cfg [:fmt :f]) %&)))))

(defn number-summary-tooltip
  [& {:keys [tooltip-field decimal-places]
      :or   {tooltip-field  :tooltip-column
             decimal-places 0}}]
  (fn [ds]
    (-> ds
        (tc/map-columns tooltip-field [:p05 :q1 :median :q3 :p95]
                        #(five-number-summary-string {:fmt (format "%%,.%df" decimal-places)} %&)))))

(defn pct-summary-tooltip
  [& {:keys [tooltip-field]
      :or {tooltip-field :tooltip-column}}]
  (fn [ds]
    (-> ds
        (tc/map-columns tooltip-field [:p05 :q1 :median :q3 :p95]
                        #(five-number-summary-string {:fmt "%.2f%%", :f (partial * 100)} %&)))))


(defn line-and-ribbon-and-rule-plot
  "Vega-Lite specs for a line, shape and ribbon plot, by `group`, with a \"hover\" vertical rule tooltip."
  ;; FIXME: It would be good to have other plots follow this adroddiad inspired form
  [{:keys [data
           chart-title
           x x-title x-format x-scale
           y y-title y-format y-scale y-zero
           orl irl iru oru
           tooltip-field tooltip-formatf
           group group-title
           chart-height chart-width
           colors-and-shapes legend]
    :or   {chart-height  full-height
           chart-width   full-width
           y-domain      false
           y-zero        true
           y-format      ",.0f"
           tooltip-field :tooltip-column
           legend        true}
    :as   plot-spec}]
  (let [tooltip-formatf       (or tooltip-formatf
                                  (five-number-summary-tooltip (assoc (select-keys plot-spec [:tooltip-field
                                                                                              :orl :irl :y :iru :oru])
                                                                      :fmt (str "%" (str/replace y-format #"%" "%%")))))
        tooltip-group-formatf (fn [g] (str g " " (->> g
                                                      (get (as-> colors-and-shapes $
                                                             (zipmap (:domain-value $) (:unicode-shape $)))))))]
    {:data     {:values (-> data
                            (tc/rows :as-maps))}
     :height   chart-height
     :width    chart-width
     :title    {:text chart-title :fontSize 24}
     :config   {:legend {:titleFontSize 20 :labelFontSize 14 :labelLimit 0}
                :axisX  {:titleFontSize 16 :labelFontSize 12}
                :axisY  {:titleFontSize 16 :labelFontSize 12}}
     :encoding {:x {:field x :title x-title :type "temporal"
                    :scale {:domain x-scale :x-xero false}
                    ;; :axis {:format ["%Y"] :tickCount {:interval "month" :step 12}}
                    }
                :color {:legend legend}}
     :layer    [{:encoding {:color (color-map data group colors-and-shapes)
                            :shape (shape-map data group colors-and-shapes)
                            :y     {:field y
                                    :type  "quantitative"
                                    :axis  {:format y-format}
                                    :scale {:domain y-scale :zero y-zero}}}
                 :layer    [{:mark {:type  "line"
                                    :size  5
                                    :point {:filled      true #_false
                                            ;; :size        150
                                            :strokewidth 0.5}}}
                            {:mark     "errorband"
                             :encoding {:y       {:field oru :title y-title :type "quantitative"}
                                        :y2      {:field orl}
                                        :color   {:field group :title group-title}
                                        :tooltip nil}}
                            {:mark     "errorband"
                             :encoding {:y       {:field iru :title y-title :type "quantitative"}
                                        :y2      {:field irl}
                                        :color   {:field group :title group-title}
                                        :tooltip nil}}
                            {:transform [{:filter {:param "hover" :empty false}}] :mark {:type "point" :size 200 :strokeWidth 5}}]}
                {:data     {:values (-> data
                                        tooltip-formatf
                                        (tc/order-by [x])
                                        (tc/select-columns [x group tooltip-field])
                                        (tc/map-columns group [group] tooltip-group-formatf)
                                        (tc/pivot->wider [group] [tooltip-field] {:drop-missing? false})
                                        (tc/replace-missing :all :value "")
                                        (tc/reorder-columns (cons x (:domain-value colors-and-shapes)))
                                        (tc/rows :as-maps))}
                 :mark     {:type "rule" :strokeWidth 4}
                 :encoding {:opacity {:condition {:value 0.3 :param "hover" :empty false}
                                      :value     0}
                            :tooltip (into [{:field x :type "temporal" :format x-format :title x-title}]
                                           (map (fn [g] {:field (tooltip-group-formatf g)}))
                                           (keep (into #{} (get data group)) (get colors-and-shapes :domain-value)))}
                 :params   [{:name   "hover"
                             :select {:type    "point"
                                      ;; :size    200
                                      :fields  [x]
                                      :nearest true
                                      :on      "pointerover"
                                      :clear   "pointerout"}}]}]}))

(defn line-and-ribbon-plot
  ;; FIXME: Update with what we've learned from adroddiad
  [{:keys [data
           chart-title chart-height chart-width
           x x-title x-format
           y y-title y-scale y-zero
           irl iru ir-title
           orl oru or-title
           group group-title
           colors-and-shapes]
    :or {chart-height full-height
         chart-width  full-width
         y-zero       true
         y-scale      false}}]
  {:height chart-height
   :width chart-width
   :title {:text chart-title
           :fontSize 24}
   :config {:legend {:titleFontSize 20
                     :labelFontSize 14}
            :axisX {:titleFontSize 16
                    :labelFontSize 12}
            :axisY {:titleFontSize 16
                    :labelFontSize 12}}
   :data {:values (-> data
                      (tc/map-columns :ir [irl iru] (fn [lower upper]
                                                      (format "%,.1f - %,.1f" lower upper)))
                      (tc/map-columns :or [orl oru] (fn [lower upper]
                                                      (format "%,.1f - %,.1f" lower upper)))
                      (tc/rows :as-maps))}
   :encoding {:y {:scale {:domain y-scale
                          :zero   y-zero}}}
   :layer [{:mark "errorband"
            :encoding {:y {:field iru :title y-title :type "quantitative"}
                       :y2 {:field irl}
                       :x {:field x :title x-title :type "temporal" :format x-format}
                       :color {:field group :title group-title}
                       :tooltip [{:field group :title group-title}
                                 {:field x :type "temporal" :format x-format :title x-title}
                                 {:field y :title y-title}
                                 {:field :ir :title ir-title}
                                 {:field :or :title or-title}]}}
           {:mark "errorband"
            :encoding {:y {:field oru :title y-title :type "quantitative"}
                       :y2 {:field orl}
                       :x {:field x :title x-title :type "temporal" :format x-format}
                       :color {:field group :title group-title}}}
           {:mark {:type "line"
                   :size 5}
            :encoding {:y {:field y :title y-title :type "quantitative"}
                       :x {:field x :title x-title :type "temporal" :axis {:format x-format}}
                       ;; color and shape scale and range must be specified or you get extra things in the legend
                       :color (color-map data group colors-and-shapes)
                       :tooltip [{:field group :title group-title}
                                 {:field x :type "temporal" :format x-format :title x-title}
                                 {:field y :title y-title}
                                 {:field :ir :title ir-title}
                                 {:field :or :title or-title}]}}]})

(defn line-shape-and-ribbon-plot
  ;; FIXME: Update with what we've learned from adroddiad
  [{:keys [data
           chart-title
           chart-height chart-width
           x x-title x-format
           y y-title y-zero y-scale
           irl iru ir-title
           orl oru or-title
           group group-title
           colors-and-shapes]
    :or   {chart-height full-height
           chart-width  full-width
           y-zero       true
           y-scale      false}}]
  {:height   chart-height
   :width    chart-width
   :title    {:text     chart-title
              :fontSize 24}
   :config   {:legend {:titleFontSize 20
                       :labelFontSize 14}
              :axisX  {:titleFontSize 16
                       :labelFontSize 12}
              :axisY  {:titleFontSize 16
                       :labelFontSize 12}}
   :data     {:values (-> data
                          (tc/map-columns :ir [irl iru] (fn [lower upper]
                                                          (format "%,.1f - %,.1f" lower upper)))
                          (tc/map-columns :or [orl oru] (fn [lower upper]
                                                          (format "%,.1f - %,.1f" lower upper)))
                          (tc/rows :as-maps))}
   :encoding {:y {:scale {:domain y-scale
                          :zero   y-zero}}}
   :layer    [{:mark     "errorband"
               :encoding {:y       {:field iru :title y-title :type "quantitative"}
                          :y2      {:field irl}
                          :x       {:field x :title x-title :type "temporal" :format x-format}
                          :color   {:field group :title group-title}
                          :tooltip [{:field group, :title group-title},
                                    {:field x, :type "temporal", :format x-format, :title x-title},
                                    {:field y, :title y-title}
                                    {:field :ir :title ir-title}
                                    {:field :or :title or-title}]}}
              {:mark     "errorband"
               :encoding {:y       {:field oru :title y-title :type "quantitative"}
                          :y2      {:field orl}
                          :x       {:field x :title x-title :type "temporal" :format x-format}
                          :color   {:field group :title group-title}
                          :tooltip [{:field group, :title group-title},
                                    {:field x, :type "temporal", :format x-format, :title x-title},
                                    {:field y, :title y-title}
                                    {:field :ir :title ir-title}
                                    {:field :or :title or-title}]}}
              {:mark     {:type "line", :point {:filled      false,
                                                :fill        "white",
                                                :size        50
                                                :strokewidth 0.5}},
               :encoding {:y       {:field y, :title y-title :type "quantitative"}
                          :x       {:field x, :title x-title :type "temporal" :axis {:format x-format}}
                          ;; color and shape scale and range must be specified or you get extra things in the legend
                          :color   (color-map data group colors-and-shapes)
                          :shape   (shape-map data group colors-and-shapes)
                          :tooltip [{:field group, :title group-title}
                                    {:field x, :type "temporal", :format x-format, :title x-title}
                                    {:field y, :title y-title}
                                    {:field :ir :title ir-title}
                                    {:field :or :title or-title}]}}]})

(def base-chart-spec
  {:y   :median
   :irl :q1  :iru :q3  :ir-title "50% range"
   :orl :p05 :oru :p95 :or-title "90% range"})
