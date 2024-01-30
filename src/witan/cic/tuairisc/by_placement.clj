(ns witan.cic.tuairisc.by-placement)

(def placement-sections
  {"Adoption" (sorted-set "A3" "A4" "A5" "A6")
   "Children's Homes" (sorted-set "K1" "K2")
   "Other" (sorted-set "P1" "P2H5" "P3" "Z1")
   "Fostering" (sorted-set "Q1" "Q2")
   "Residential" (sorted-set "R1" "R2" "R3" "R5" "S1")})

(def placement-colors-and-shapes
  {"A3" {:color [174.0, 199.0, 232.0, 255.0], :shape \|, :legend-shape \|}
   "A4" {:color [227.0, 119.0, 194.0, 255.0], :shape \S, :legend-shape \S}
   "A5" {:color [152.0, 223.0, 138.0, 255.0], :shape \V, :legend-shape \A}
   "A6" {:color [44.0, 160.0, 44.0, 255.0], :shape \O, :legend-shape \O}
   "K1" {:color [197.0, 176.0, 213.0, 255.0], :shape \>, :legend-shape \>}
   "K2" {:color [196.0, 156.0, 148.0, 255.0], :shape \{, :legend-shape \{}
   "P1" {:color [199.0, 199.0, 199.0, 255.0], :shape \-, :legend-shape \-}
   "P2H5" {:color [255.0, 127.0, 14.0, 255.0], :shape \^, :legend-shape \v}
   "P3" {:color [127.0, 127.0, 127.0, 255.0], :shape \<, :legend-shape \<}
   "Q1" {:color [247.0, 182.0, 210.0, 255.0], :shape \v, :legend-shape \^}
   "Q2" {:color [148.0, 103.0, 189.0, 255.0], :shape \x, :legend-shape \x}
   "R1" {:color [219.0, 219.0, 141.0, 255.0], :shape \}, :legend-shape \}}
   "R2" {:color [214.0, 39.0, 40.0, 255.0], :shape \A, :legend-shape \V}
   "R3" {:color [255.0, 187.0, 120.0, 255.0], :shape \\, :legend-shape \/}
   "R5" {:color [31.0, 119.0, 180.0, 255.0], :shape \/, :legend-shape \\}
   "S1" {:color [140.0, 86.0, 75.0, 255.0], :shape \s, :legend-shape \s}
   "Z1" {:color [255.0, 152.0, 150.0, 255.0], :shape \o, :legend-shape \o}})
