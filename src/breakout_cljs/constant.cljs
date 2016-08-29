(ns breakout-cljs.constant
  (:require [goog.dom :as dom]))

(def ^:const canvas (dom/getElement "game"))
(def ^:const width (.-width canvas))
(def ^:const height (.-height canvas))
(def ^:const gutter 10)
(def ^:const block-health 3)
(def ^:const block-offset 20)

(def ^:const colors ["#FBF2C0" "#F7B538" "#D8572A" "#C32F27" "#69140E"])
