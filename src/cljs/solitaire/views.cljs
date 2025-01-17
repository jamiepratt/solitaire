(ns solitaire.views
  (:require [clojure.string :as string]
            [re-frame.core :as rf]
            [solitaire.events :as events]
            [solitaire.subs :as subs]))

(defn field-blocked []
  [:div.field.field--blocked])

(defn field-empty [{:keys [x y]}]
  [:div.field {:on-click #(rf/dispatch [::events/make-move x y])}])

(defn field-peg [{:keys [selected? x y]}]
  [:div.field
   [:div.peg (merge {:class (when selected? "peg--selected")}
                    (when (= :in-progress @(rf/subscribe [::subs/status])) 
                      {:on-click #(rf/dispatch [::events/select-field x y])}))]])

(defn field-view [x y]
  (let [{:keys [type] :as field} @(rf/subscribe [::subs/field x y])]
    (case type
      :blocked [field-blocked]
      :empty [field-empty field]
      :peg [field-peg field])))

(defn board-view []
  (let [[width height] @(rf/subscribe [::subs/board-dimensions])
        playing (= :in-progress @(rf/subscribe [::subs/status]))]
    (into
     [:div.board
      {:style (merge {:grid-template-columns (string/join " " (repeat width "1fr"))
                      :grid-template-rows (string/join " " (repeat height "1fr"))}
                     (when (not playing) {:opacity "40%"}))}]
      (for [y (range height)
            x (range width)]
        [field-view x y]))))

(defn pegs-count []
  [:h1 "Pegs left: " @(rf/subscribe [::subs/pegs-count])])

(defn game []
  [:div
   [pegs-count]
   [:button (merge {:on-click (fn [_] (rf/dispatch [:undo]))}
                   (when (not @(rf/subscribe [:undos?])) {:disabled true})) "Undo"]
   [:button (merge {:on-click (fn [_] (rf/dispatch [:redo]))}
                   (when (not @(rf/subscribe [:redos?])) {:disabled true})) "Redo"]
   [board-view]])

(defn play-button []
  [:button {:on-click #(rf/dispatch [::events/start])} "Start game"])

(defn play-again-button []
  [:button {:on-click #(rf/dispatch [::events/again])} "Play again"])


(defn game-over []
  [:div
   [:h1 "Game over, " @(rf/subscribe [::subs/pegs-count]) " pegs left"]
   [play-again-button]
   [board-view]])

(defn change-board []
  [:button {:on-click #(rf/dispatch [::events/change-board])} "Change board"])


(defn menu []
  [:div.menu
   [:div
    [:h1 "Welcome to Solitaire!"]
    [play-button][change-board]]
    [board-view]])

(defn main-panel []
  [:div
   (case @(rf/subscribe [::subs/status])
     :not-started [menu]
     :game-over   [game-over]
     :in-progress [game]
     nil)])
