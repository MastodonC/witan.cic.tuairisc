(ns witan.cic.tuairisc.by-age)

(def age-sections
  {"0-5" (sorted-set 0 1 2 3 4 5)
   "6-11" (sorted-set 6 7 8 9 10 11)
   "12-17" (sorted-set 12 13 14 15 16 17)})

(def age-colors-and-shapes
  {0  {:color [31.0, 119.0, 180.0, 255.0], :shape \/, :legend-shape \\}
   1  {:color [255.0, 152.0, 150.0, 255.0], :shape \o, :legend-shape \o}
   2  {:color [214.0, 39.0, 40.0, 255.0], :shape \A, :legend-shape \V}
   3  {:color [197.0, 176.0, 213.0, 255.0], :shape \>, :legend-shape \>}
   4  {:color [148.0, 103.0, 189.0, 255.0], :shape \x, :legend-shape \x}
   5  {:color [247.0, 182.0, 210.0, 255.0], :shape \v, :legend-shape \^}
   6  {:color [227.0, 119.0, 194.0, 255.0], :shape \S, :legend-shape \S}
   7  {:color [196.0, 156.0, 148.0, 255.0], :shape \{, :legend-shape \{}
   8  {:color [140.0, 86.0, 75.0, 255.0], :shape \s, :legend-shape \s}
   9  {:color [127.0, 127.0, 127.0, 255.0], :shape \<, :legend-shape \<}
   10 {:color [219.0, 219.0, 141.0, 255.0], :shape \}, :legend-shape \}}
   11 {:color [199.0, 199.0, 199.0, 255.0], :shape \-, :legend-shape \-}
   12 {:color [188.0, 189.0, 34.0, 255.0], :shape \V, :legend-shape \A}
   13 {:color [158.0, 218.0, 229.0, 255.0], :shape \\, :legend-shape \/}
   14 {:color [23.0, 190.0, 207.0, 255.0], :shape \^, :legend-shape \v}
   15 {:color [152.0, 223.0, 138.0, 255.0], :shape \|, :legend-shape \|}
   16 {:color [255.0, 187.0, 120.0, 255.0], :shape \O, :legend-shape \O}
   17 {:color [255.0, 127.0, 14.0, 255.0], :shape \/, :legend-shape \\}
   18 {:color [174.0, 199.0, 232.0, 255.0], :shape \o, :legend-shape \o}})

(def age-group {0 "Age 0"
                1 "Age 1-5"
                2 "Age 1-5"
                3 "Age 1-5"
                4 "Age 1-5"
                5 "Age 1-5"
                6 "Age 6-10"
                7 "Age 6-10"
                8 "Age 6-10"
                9 "Age 6-10"
                10 "Age 6-10"
                11 "Age 11-15"
                12 "Age 11-15"
                13 "Age 11-15"
                14 "Age 11-15"
                15 "Age 11-15"
                16 "Age 16"
                17 "Age 17"})

(defn broad-age-group [age]
  (cond
    (< age 16) "Under 16"
    (<= 16 age) "16+"))
