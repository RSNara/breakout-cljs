(ns breakout-cljs.core
  (:require [breakout-cljs.util :refer [clamp abs trace floor]]
            [breakout-cljs.constant :refer [canvas width height gutter block-health block-offset]]
            [breakout-cljs.protocol :refer [Mortal Moveable Collidable intersect move collide alive?]]
            [breakout-cljs.entity :as Entity]
            [breakout-cljs.system :as System]
            [goog.events :as events]))

(enable-console-print!)

(defn create-ball [paddle]
  (let [paddle-p (:p paddle)
        paddle-dim (:dim paddle)
        ball-dim (map #(identity 10) paddle-dim)
        ball-p (map - paddle-p (cons 0 (rest ball-dim)))
        ball-v (map #(identity 4) (:v paddle))
        ball-a (map #(identity 0) (:a paddle))
        ball-health js/Infinity]
    (Entity/Block. ball-dim ball-p ball-v ball-a ball-health)))

(defn create-paddle []
  (Entity/Block. [80 10] [40 (- height 10 5)] [0 0] [0 0] js/Infinity))

(defn get-block-dim [total-space num]
  (/ (- total-space (* gutter (inc num))) num))

(defn get-block-p [index dim]
  (+ (* (inc index) gutter) (* index dim)))

(defn create-blocks [ columns rows]
  (let [nums [columns rows]
        container [width (/ height 3)]
        size (map get-block-dim container nums)]
    (for [i (range 0 columns)
          j (range 0 rows)]
      (Entity/Block. size (map get-block-p [i j] size) [0 0] [0 0] block-health))))

(defn translate-blocks [blocks p]
  (map #(update % :p (partial map + p)) blocks))

(defn initial-state []
  (let [paddle (create-paddle)]
    {:blocks (translate-blocks (create-blocks 8 6) [0 block-offset])
     :paddle paddle
     :ball (create-ball paddle)
     :keys #{}
     :lives 3
     :score 0}))

(defonce game-state (atom (initial-state)))

(defn game-loop [now]
  (.requestAnimationFrame js/window game-loop)
  (let [state @game-state]
    (if (-> state :lives pos?)
      (->> state
           System/Collider
           System/ScoreUpdater
           System/DeadBlockRemover
           System/Mover
           System/GameLifeDecrementer
           System/PaddleMover
           System/WorldEdgeBouncer
           System/WorldClamper
           System/Renderer!
           (reset! game-state))
      (reset! game-state (initial-state)))))

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

