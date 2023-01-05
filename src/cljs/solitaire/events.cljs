(ns solitaire.events
  (:require
   [re-frame.core :as rf]
   [solitaire.board :as board]
   [solitaire.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   [day8.re-frame.undo :as undo]))

(rf/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
            (assoc db/default-db
                   :board (board/initial-board-no (:board-no db/default-db)))))
(rf/reg-event-db
 ::start
 (fn [db _]
   (assoc db :status :in-progress)))

(rf/reg-event-db
 ::change-board
 (fn [{:keys [board-no] :as db} _]
   (if-let [next-board (board/initial-board-no (inc board-no))]
     (assoc db :board next-board
               :board-no (inc board-no))
     (assoc db :board (board/initial-board-no 0)
               :board-no 0))))

(rf/reg-event-db
 ::again
 (fn [{:keys [board-no] :as db} _]
   (assoc db
          :board (board/initial-board-no board-no)
          :status :not-started)))


(rf/reg-event-db
 ::select-field
 (fn [db [_ x y]]
   (assoc db :selected-field
          (if (= (:selected-field db) [x y])
            nil
            [x y]))))

(rf/reg-event-fx
 ::end-game
 (fn [{db :db} _]
   {:db (assoc db :status :game-over)}))

(rf/reg-event-fx
 ::make-move
 (undo/undoable "move")
 (fn [{{:keys [board] :as db} :db} [_ x y]]
   (let [source (:selected-field db)
         target [x y]]
     (if (board/can-move? board source target)
       (let [new-board (board/move board source target)]
         (merge {:db (assoc db
                            :board new-board
                            :selected-field nil)}
                (when (board/game-over? new-board)
                  {:dispatch [::end-game]})))
       {:db (assoc db :selected-field nil)}))))
