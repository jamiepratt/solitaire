(ns solitaire.board)

;;; Accessing the board

(defn dimensions [board]
  [(count (first board)) (count board)])

(defn coord-seq [board]
  (let [[width height] (dimensions board)]
    (for [y (range height) x (range width)]
      [x y])))

(defn field-at [board [x y]]
  (get-in board [y x]))

(defn update-field [board [x y] item]
  (assoc-in board [y x] item))

(defn count-pegs [board]
  (count (filter #{:peg} (flatten board))))

;;; Game rules

(defn midpoint [[x y] [x' y']]
  (cond
    (and (= x' x) (= y' (- y 2))) [x (- y 1)]
    (and (= x' x) (= y' (+ y 2))) [x (+ y 1)]
    (and (= x' (- x 2)) (= y' y)) [(- x 1) y]
    (and (= x' (+ x 2)) (= y' y)) [(+ x 1) y]
    :otherwise                    nil))

(defn can-move? [board source target]
  (and (= (field-at board source) :peg)
       (= (field-at board target) :empty)
       (when-let [mid (midpoint source target)]
         (= (field-at board mid) :peg))))

(defn move [board source target]
  (let [mid (midpoint source target)]
    (-> board
        (update-field source :empty)
        (update-field mid :empty)
        (update-field target :peg))))

(defn potential-targets [[x y]]
  [[x (- y 2)]
   [x (+ y 2)]
   [(- x 2) y]
   [(+ x 2) y]])

(defn legal-targets [board source]
  (filter (partial can-move? board source)
          (potential-targets source)))

(defn can-move-anywhere? [board source]
  (first (legal-targets board source)))

(defn pieces-that-can-move [board]
  (filter (partial can-move-anywhere? board)
          (coord-seq board)))

(defn game-over? [board]
  (empty? (pieces-that-can-move board)))


(defn find-possible-next-moves [board]
  (for [source (pieces-that-can-move board)
        target (legal-targets board source)]
    [source target]))

(defn all-possible-series-of-moves
  [board moves-so-far]
  (let [next-moves (find-possible-next-moves board)
        next-boards (map (fn [[s t]] (move board s t)) next-moves)]
    (mapcat (fn [next-board move]
              (when (= board next-board)
                (println next-board)
                (println move))
              (let [next-moves-so-far (conj moves-so-far move)]
                (if (game-over? next-board)
                  [next-moves-so-far]
                  (all-possible-series-of-moves next-board next-moves-so-far))))
            next-boards next-moves)))

