(ns witan.cic.tuairisc.by-age)

;; This is where charting will go

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
