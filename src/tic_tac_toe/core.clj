(ns tic-tac-toe.core)

(def initial-board [[1 2 3] [4 5 6] [7 8 9]])

;todo - check how is the winner

(defn print-board [board]
  (println (str " " (get-in board [0 0]) " | " (get-in board [0 1]) " | " (get-in board [0 2]) " "))
  (println (str "-----------"))
  (println (str " " (get-in board [1 0]) " | " (get-in board [1 1]) " | " (get-in board [1 2]) " "))
  (println (str "-----------"))
  (println (str " " (get-in board [2 0]) " | " (get-in board [2 1]) " | " (get-in board [2 2]) " ")))

(defn player-turn [player-x-turn]
  (if (= player-x-turn true)
    "x"
    "o"))

(defn free-field? [input board]
  (cond
    (contains? #{1 2 3} input) (number? (get-in board [0 (rem (dec input) 3)]))
    (contains? #{4 5 6} input) (number? (get-in board [1 (rem (dec input) 3)]))
    (contains? #{7 8 9} input) (number? (get-in board [2 (rem (dec input) 3)]))))

(defn valid-field? [input board]
  (if (free-field? input board)
    true
    (do
      (println "!!!!!!!!Invalid move!! Space " input " is already taken. Try again.")
      false)))

(defn convert-to-column-order [board]
  (vec (map
         (fn [index] (vec (map
                            (fn [v] (v index))
                            board)))
         (range 0 3))))
(defn convert-to-diagonal-order [board]
  (let [from-top-left (vec (map (fn [i] (get-in board [i i])) (range 0 3)))
        from-top-right (vec (map (fn [i] (get-in (vec (reverse board)) [i i])) (range 0 3)))]
    (conj [] from-top-left from-top-right)))

(defn winner? [board player]
  (let [win-on-row? (some (fn [row] (every? (fn [e] (= e player)) row)) board)
        row board
        column (convert-to-column-order board)
        diagonal (convert-to-diagonal-order board)
        fun (->> row
                 (concat column diagonal)
                 vec
                 (some (fn [row] (every? (fn [e] (= e player)) row))))]
    (do
      (println "res="fun "player=" player board )
      fun)))

(defn tie?
  [board]
  (let [full-board? (every?
                      (fn [row] (not-any? number? row))
                      board)
        available-space (reduce
                          (fn [acc r]
                            (+ acc
                               (reduce
                                 (fn [acc x] (if (number? x) (inc acc) 0))
                                 0
                                 r)))
                          0
                          board)]
    ;(if (<= available-space 1)
    ;  (do
    ;    (println "The game is a tie!!!. Start the game to play again.")
    ;    true)
    ;  false)
    full-board?
    ))

(defn move [input board player-x-turn?]
  (cond
    (contains? #{1 2 3} input) (assoc board 0 (assoc (get-in board [0]) (rem (dec input) 3) (player-turn player-x-turn?)))
    (contains? #{4 5 6} input) (assoc board 1 (assoc (get-in board [1]) (rem (dec input) 3) (player-turn player-x-turn?)))
    (contains? #{7 8 9} input) (assoc board 2 (assoc (get-in board [2]) (rem (dec input) 3) (player-turn player-x-turn?)))))
(defn valid-number?
  "Returns true if the `input` is an integer between 1-9, else returns false.
  If the input is a string an exception is thrown"
  [number]
  (if (contains? (set (range 1 10)) number)
    true
    (do
      (println "!!!!!!!!!!! Invalid input! You most provide a number between 1-9. Try again.!!!!!!!!!!!")
      false)))

(defn read-input []
  (let [input (read-line)]
    (try
      (if (= input "stop")
        "stop"
        (Integer/parseInt input))
      (catch NumberFormatException e
        (println "!!!!!!!!!!!Input most be an number!!!!!!!!!!!")))))

(defn take-turn [player-x-turn?]
  (if (= player-x-turn? true)
    (do
      (println "It is players x's turn")    ;move player x
      true)
    (do
      (println "It is players o's turn") ;else mover player o
      false)))

(defn start
  []
  (println "You have started a new game. Press a number between 1-9")
  (loop [board initial-board                                ;state of board
         player-x-turn? false                                ;shows players turn it is.
         ]
    (let [print-board (print-board board)
          input (read-input)]
      (cond
        (= input "stop") (str "game has stopped")               ;End the game if user writes stop.
        (winner? board (player-turn player-x-turn?)) (do
                                                       print-board
                                                       (str "You have won the game!"))
        (tie? board)    (do
                          print-board
                          (str "The game is a tie"))
        (and (valid-number? input) (valid-field? input board)) (recur
                                                                 (move input board player-x-turn?) ;state of board
                                                                 (take-turn (not player-x-turn?))) ;each player take turns
        :else
        (recur
          board
          (take-turn player-x-turn?)                        ;take turn.
          )))))
