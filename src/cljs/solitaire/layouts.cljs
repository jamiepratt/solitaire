(ns solitaire.layouts)


;;; Layout
(def layouts
  '[[[. . o]
    [. . o]
    [o o _]]
    [[. o o ]
     [o o o ]
     [o o _ ]]
    [[. . o o ]
     [. . o o ]
     [o o o o ]
     [o o o _ ]]
    [[. . o o ]
     [. o o o ]
     [o o o o ]
     [o o o _ ]]
    [[. . . . . o]
     [. . . . o o]
     [. . . o o o]
     [. . o o o o]
     [. o o o o o]
     [o o o o o _]]
    ])

(comment (distinct (flatten layouts))
         (map #(map count %) layouts))

(defn layout->board
  "Assuming a symmetrical layout we can take just the top left corner of the
   board and mirror it left and right and top and bottom."
  [layout]
  (->>
   layout
   (map (partial map {'. :blocked, 'o :peg, '_ :empty}))
   (map #(vec (concat % (reverse (butlast %)))))
   (#(vec (concat % (reverse (butlast %)))))))

(comment 
  (def boards (map layout->board layouts))
  (distinct (flatten boards))
         (map #(map count %) boards))


(comment (def l '[[. . o]
                 [. . o]
                 [o o _]])
         (layout->board l)
         )

(defn initial-board [n]
  (when-let [layout (get layouts n)]
    (layout->board layout)))
