(ns breakout-cljs.core
  (:require [goog.dom :as dom]
            [goog.events :as events]))

(enable-console-print!)

(println "This text is printed from src/breakout-cljs/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce ^:const canvas (dom/getElement "game"))
(defonce ^:const width (.-width canvas))
(defonce ^:const height (.-height canvas))

(defprotocol Mortal
  (alive? [this]))

(defprotocol Moveable
  (move [this t]))

(defprotocol Collidable
  (collide [this other])
  (intersect [this other]))

(defn trace [ result ] (do (println result) result))
(defn floor [x] (.floor js/Math x))

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


(defn create-ball [paddle]
  (let [paddle-p (:p paddle)
        paddle-dim (:dim paddle)
        ball-dim (map #(identity 10) paddle-dim)
        ball-p (map - paddle-p ball-dim)
        ball-v (map #(identity 4) (:v paddle))
        ball-a (map #(identity 0) (:a paddle))
        ball-health js/Infinity]
    (Block. ball-dim ball-p ball-v ball-a ball-health)))

(defn create-paddle []
  (Block. [40 10] [40 (- height 10 5)] [0 0] [0 0] js/Infinity))

(defn get-block-dim [gutter total-space num]
  (/ (- total-space (* gutter (inc num))) num))

(defn get-block-p [gutter index dim]
  (+ (* (inc index) gutter) (* index dim)))

(defn create-blocks [ columns rows]
  (let [gutter 10
        nums [columns rows]
        container [width (/ height 3)]
        size (map (partial get-block-dim gutter) container nums)
        get-block-p-with-gutter (partial get-block-p gutter)]
    (for [i (range 0 columns) j (range 0 rows)]
      (Block. size (map get-block-p-with-gutter [i j] size) [0 0] [0 0] 2))))

(defonce game-state (atom (let [paddle (create-paddle)]
                            {:blocks (create-blocks 8 6)
                             :paddle paddle
                             :ball (trace (create-ball paddle))
                             :keys #{}})))

(defn abs [x] (.abs js/Math x))

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

(defn clamp [min max x]
  (cond
    (< x min) min
    (< max x) max
    :else x))

(defn clamp-block-to-canvas [block]
  (let [p (:p block)
        max-p (map - [width height] (:dim block))
        min-p (map #(identity 0) p)
        clamped-p (map clamp min-p max-p p)]
    (assoc block :p clamped-p)))

(defn WorldClamper [state]
  (merge state {:blocks (map clamp-block-to-canvas (:blocks state))
                :paddle (clamp-block-to-canvas (:paddle state))
                :ball (clamp-block-to-canvas (:ball state))}))

(defn bounce-block-off-world-edges [block]
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

(defn render-block! [context block]
  (let [[x y] (map floor (:p block))
        [width height] (map floor (:dim block))]
    (.fillRect context x y width height)))

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

(defn game-loop [now]
  (.requestAnimationFrame js/window game-loop)
  (->> @game-state
       Collider
       DeadBlockRemover
       Mover
       PaddleMover
       WorldEdgeBouncer
       WorldClamper
       Renderer!
       (reset! game-state)))

(defn init []
  (letfn [(get-code [event]
            (-> event (aget "event_") (aget "code") keyword))]
    (events/listen js/window
                   (.-KEYDOWN events/EventType)
                   (fn [event]
                     (swap! game-state update :keys #(conj % (get-code event)))
                     (.preventDefault event)))
    (events/listen js/window
                   (.-KEYUP events/EventType)
                   (fn [event]
                     (swap! game-state update :keys #(disj % (get-code event)))
                     (.preventDefault event)))
    (game-loop (.now js/performance))))

(defonce start (init))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

