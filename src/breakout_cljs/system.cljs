(ns breakout-cljs.system
  (:require [breakout-cljs.protocol :refer [intersect move collide alive?]]
            [breakout-cljs.constant :refer [canvas width height]]
            [breakout-cljs.util :refer [clamp abs floor]]))

(defn Mover [state]
  (let [time 1
        ball (:ball state)
        paddle (:paddle state)]
    (merge state {:ball (move ball time)
                  :paddle (move paddle time)})))

(defn Collider [state]
  (let [blocks (:blocks state)
        paddle (:paddle state)
        ball (:ball state)]
    (merge state {:blocks (map #(collide % ball) blocks)
                  :ball (-> (reduce collide ball blocks)
                            (collide paddle))})))

(defn- clamp-block-to-canvas [block]
  (let [p (:p block)
        max-p (map - [width height] (:dim block))
        min-p (map #(identity 0) p)
        clamped-p (map clamp min-p max-p p)]
    (assoc block :p clamped-p)))

(defn WorldClamper [state]
  (merge state {:blocks (map clamp-block-to-canvas (:blocks state))
                :paddle (clamp-block-to-canvas (:paddle state))
                :ball (clamp-block-to-canvas (:ball state))}))

(defn- bounce-block-off-world-edges [block]
  (let [[x-end y-end] (map + (:p block) (:dim block))
        [x y] (:p block)
        [vx vy] (:v block)]
    (cond
      (< x 0) (assoc block :v [(abs vx) vy])
      (< y 0) (assoc block :v [vx (abs vy)])
      (< width x-end) (assoc block :v [(->> vx abs (- 0)) vy])
      (< height y-end) (assoc block :v [vx (->> vy abs (- 0))])
      :else block)))

(defn WorldEdgeBouncer [state]
  (let [ball (:ball state)]
    (merge state {:ball (bounce-block-off-world-edges ball)})))

(defn PaddleMover [state]
  (let [speed 4
        keys-pressed (:keys state)
        paddle (:paddle state)
        v (:v paddle)]
    (->> (cond
           (:ArrowLeft keys-pressed) (assoc paddle :v (cons (unchecked-negate speed) (rest v)))
           (:ArrowRight keys-pressed) (assoc paddle :v (cons speed (rest v)))
           :else (assoc paddle :v (cons 0 (rest v))))
         (assoc state :paddle))))

(defn DeadBlockRemover [state]
  (->> (:blocks state)
       (filter alive?)
       (assoc state :blocks)))

(defn- render-block! [context block]
  (let [[x y] (map floor (:p block))
        [width height] (map floor (:dim block))]
    (.fillRect context x y width height)))

(defn Renderer! [state]
  (let [blocks (:blocks state)
        paddle (:paddle state)
        ball (:ball state)
        context (.getContext canvas "2d")]
    (.clearRect context 0 0 width height)
    (aset context "fillStyle" "#000")
    (doseq [block blocks] (render-block! context block))
    (render-block! context paddle)
    (render-block! context ball))
  state)