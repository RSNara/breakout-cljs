(ns breakout-cljs.constant
  (:require [goog.dom :as dom]))

(def ^:const canvas (dom/getElement "game"))
(def ^:const width (.-width canvas))
(def ^:const height (.-height canvas))