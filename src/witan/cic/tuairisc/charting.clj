(ns witan.cic.tuairisc.charting
  (:require [tablecloth.api :as tc]))

(def full-height 800)
(def full-width 1200)

(defn color-and-shape-lookup [domain]
  (tc/dataset {:domain-value domain
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


(defn line-and-ribbon-and-rule-plot [{:keys [data
                                             chart-title
                                             x
                                             y
                                             group
                                             height width]
                                      :or {height 200
                                           width 1000}}]
  {:data {:values (-> data
                      (tc/rows :as-maps))}
   :height height
   :width width
   :title {:text chart-title :fontSize 24}
   :config {:legend {:titleFontSize 20 :labelFontSize 14}
            :axisX {:titleFontSize 16 :labelFontSize 12}
            :axisY {:titleFontSize 16 :labelFontSize 12}}
   :encoding {:x {:field x :type "temporal"}}
   :layer [{:encoding {:color {:field group :type "nominal"}
                       :y {:field y :type "quantitative" :scale {:domain false :zero false}}}
            :layer [{:mark {:type "line" :size 5}}
                    {:transform [{:filter {:param "hover" :empty false}}] :mark "point"}]}
           {:transform [{:pivot group :value y :groupby [x]}]
            :mark "rule"
            :encoding {:opacity {:condition {:value 0.3 :param "hover" :empty false}
                                 :value 0}
                       :tooltip (into []
                                      (map (fn [g] {:field g :type "quantitative"}))
                                      (into (sorted-set) (data group)))}
            :params [{:name "hover"
                      :select {:type "point"
                               :fields [x]
                               :nearest true
                               :on "pointerover"
                               :clear "pointerout"}}]}]})

(defn line-and-ribbon-plot
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
