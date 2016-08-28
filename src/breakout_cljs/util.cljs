(ns breakout-cljs.util)

(defn clamp [min max x]
  (cond
    (< x min) min
    (< max x) max
    :else x))

(defn abs [x] (.abs js/Math x))

(defn trace [ result ] (do (println result) result))
(defn floor [x] (.floor js/Math x))