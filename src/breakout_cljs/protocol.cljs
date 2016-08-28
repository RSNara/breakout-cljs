(ns breakout-cljs.protocol)

(defprotocol Mortal
  (alive? [this]))

(defprotocol Moveable
  (move [this t]))

(defprotocol Collidable
  (collide [this other])
  (intersect [this other]))