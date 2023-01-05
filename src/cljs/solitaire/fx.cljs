(ns solitaire.fx
  (:require [re-frame.core :as rf]
            [day8.re-frame.undo :as undo]))

(rf/reg-fx
 :alert
 (fn [message]
   (js/alert message)))

(rf/reg-fx
 :clear-undo-history
 (fn []
   (println "clearinf")
   (undo/clear-undos!)))
