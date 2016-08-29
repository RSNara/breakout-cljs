(ns breakout-cljs.system
  (:require [breakout-cljs.protocol :refer [intersect move collide alive?]]
            [breakout-cljs.constant :refer [canvas width height colors gutter]]
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
        acceleration 1
        keys-pressed (:keys state)
        paddle (:paddle state)
        v (:v paddle)
        a (:a paddle)]
    (->> (cond
           (:ArrowLeft keys-pressed) (merge paddle {:v (cons (unchecked-negate speed) (rest v))
                                                    :a (cons (unchecked-negate acceleration) (rest a))})
           (:ArrowRight keys-pressed) (merge paddle {:v (cons speed (rest v))
                                                     :a (cons acceleration (rest a))})
           :else (merge paddle {:v (cons 0 (rest v)) :a (cons 0 (rest a))}))
         (assoc state :paddle))))

(defn DeadBlockRemover [state]
  (->> (:blocks state)
       (filter alive?)
       (assoc state :blocks)))

(defn- render-block-rect! [context block]
  (let [color (get colors (:health block) "#000")
        [x y] (map floor (:p block))
        [width height] (map floor (:dim block))]
    (aset context "fillStyle" color)
    (.fillRect context x y width height)))

(defn- render-circle! [context x y radius]
  (.beginPath context)
  (.arc context x y radius 0 (* 2 (.-PI js/Math)) false)
  (.fill context))

(defn- render-ellipse! [context x y x-radius y-radius]
  (.beginPath context)
  (.ellipse context x y x-radius y-radius 0 0 (* 2 (.-PI js/Math)) false)
  (.fill context))

(defn- render-block-ellipse! [context block]
  (let [p (:p block)
        dim (:dim block)
        radii (map #(/ % 2) dim)
        [x y] (map + p radii)
        [radius-x radius-y] radii]
    (render-ellipse! context x y radius-x radius-y)))

(defn Renderer! [state]
  (let [radius 5
        score (:score state)
        lives (:lives state)
        blocks (:blocks state)
        paddle (:paddle state)
        ball (:ball state)
        context (.getContext canvas "2d")]
    (.clearRect context 0 0 width height)
    (aset context "fillStyle" "#f00")
    (doseq [i (map #(- width (* % (+ gutter radius))) (range 1 (inc lives)))]
      (render-circle! context i (+ gutter radius) radius))
    (aset context "fillStyle" "#000")
    (aset context "font" "15px serif")
    (.fillText context (str score) 10 20)
    (doseq [block blocks] (render-block-rect! context block))
    (render-block-rect! context paddle)
    (render-block-ellipse! context ball))
  state)

(defn ScoreUpdater [state]
  (let [blocks (:blocks state)
        dead-blocks (filter (complement alive?) blocks)
        num-dead (count dead-blocks)]
    (update state :score + (* num-dead 1000))))


(defn GameLifeDecrementer [state]
  (let [ball (:ball state)
        ball-max-y (+ (nth (:p ball) 1) (nth (:dim ball) 1))
        lives (:lives state)]
    (assoc state :lives (if (< height ball-max-y)
                          (max (dec lives) 0)
                          lives))))