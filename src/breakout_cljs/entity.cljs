(ns breakout-cljs.entity
  (:require [breakout-cljs.protocol :refer [Mortal Moveable Collidable collide intersect]]))

(defrecord Block [dim p v a health]
  Mortal
  (alive? [this]
    (-> this :health pos?))

  Moveable
  (move [this t]
    (let [p' (map + (map #(* 0.5 t %) a) (map #(* t %) v) p)
          v' (map + (map #(* t %) a) v)]
      (merge this {:p p' :v v'})))

  Collidable
  (collide [this other]
    (if (identical? this other)
      this
      (let [intersection (intersect this other)
            min-dimension (apply min intersection)
            min-index (.indexOf intersection min-dimension)]
        (if (pos? min-dimension)
          (let [health' (dec (:health this))
                v' (map-indexed #(if (= min-index %1) (unchecked-negate %2) %2) v)]
            (merge this {:v v' :health health'}))
          this))))
  (intersect [this other]
    (let [min-p (map max p (:p other))
          max-p (map min (map + dim p) (map + (:dim other) (:p other)))
          intersection (map - max-p min-p)]
      intersection)))