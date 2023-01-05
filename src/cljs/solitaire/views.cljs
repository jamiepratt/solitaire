(ns solitaire.views
  (:require [clojure.string :as string]
            [re-frame.core :as rf]
            [solitaire.events :as events]
            [solitaire.subs :as subs]))

(defn field-blocked []
  [:div.field.field--blocked])

(defn field-empty [{:keys [x y]}]
  [:div.field {:on-click #(rf/dispatch [::events/make-move x y])}])

(defn field-peg [{:keys [selected? x y]} clickable]
  [:div.field
   [:div.peg (merge {:class (when selected? "peg--selected")}
                    (when clickable 
                      {:on-click #(rf/dispatch [::events/select-field x y])}))]])

(defn field-view [{:keys [type] :as field} clickable]
  (case type
    :blocked [field-blocked]
    :empty [field-empty field]
    :peg [field-peg field clickable]))

(defn board-view [fields-clickable]
  (let [[width height] @(rf/subscribe [::subs/board-dimensions])]
    (into
     [:div.board
      {:style {:grid-template-columns (string/join " " (repeat width "1fr"))
               :grid-template-rows (string/join " " (repeat height "1fr"))}}]
     (for [y (range height)
           x (range width)]
       [field-view @(rf/subscribe [::subs/field x y]) fields-clickable]))))

(defn pegs-count []
  [:h1 "Pegs left: " @(rf/subscribe [::subs/pegs-count])])

(defn game []
  [:div
   [pegs-count]
   [:button (merge {:on-click (fn [_] (rf/dispatch [:undo]))}
                   (when (not @(rf/subscribe [:undos?])) {:disabled true})) "Undo"]
   [:button (merge {:on-click (fn [_] (rf/dispatch [:redo]))}
                   (when (not @(rf/subscribe [:redos?])) {:disabled true})) "Redo"]
   [board-view true]])

(defn play-button []
  [:button {:on-click #(rf/dispatch [::events/start])} "Start game"])

(defn play-again-button []
  [:button {:on-click #(rf/dispatch [::events/again])} "Play again"])


(defn game-over []
  [:div
   [:h1 "Game over, " @(rf/subscribe [::subs/pegs-count]) " pegs left"]
   [play-again-button]
   [board-view false]])

(defn change-board []
  [:button {:on-click #(rf/dispatch [::events/change-board])} "Change board"])


(defn menu []
  [:div.menu
   [:div
    [:h1 "Welcome to Solitaire!"]
    [play-button][change-board]]
    [board-view false]])

(defn main-panel []
  [:div
   (case @(rf/subscribe [::subs/status])
     :not-started [menu]
     :game-over   [game-over]
     :in-progress [game]
     nil)])
