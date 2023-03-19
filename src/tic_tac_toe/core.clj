(ns tic-tac-toe.core
  (:gen-class))

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
    "X"
    "O"))

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
  (let [row board
        column (convert-to-column-order board)
        diagonal (convert-to-diagonal-order board)
        fun (->> row
                 (concat column diagonal)
                 vec
                 (some (fn [r] (every? (fn [e] (= e player)) r))))]
    fun))

(defn tie?
  [board]
  (let [full-board? (every?
                      (fn [row] (not-any? number? row))
                      board)
        ]
    full-board?))

(defn update-board [input board player-x-turn?]
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
      (println "Invalid move! You most provide a number between 1-9. Try again.")
      false)))

(defn read-input []
  (let [input (read-line)]
    (try
      (if (= input "stop")
        "stop"
        (Integer/parseInt input))
      (catch NumberFormatException e
        (println "Invalid move! Input most be an number. Try again.")))))

(defn take-turn [player-x-turn?]
  (if (= player-x-turn? true)
    (do
      (println "It is players " (player-turn player-x-turn?) "'s turn")    ;move player x
      true)
    (do
      (println "It is players " (player-turn player-x-turn?) "'s turn") ;else mover player o
      false)))

(defn move
  [board player-x?]
  (loop [board board
         player-x? player-x?]
    (let [input (read-input)]
      (cond
        (= input "stop") (str "Game has stopped")
        (and (valid-number? input) (valid-field? input board)) (update-board input board player-x?)
        :else
        (recur board player-x?)))))

(defn start-game []
  (println "You have started a new game. Press a number between 1-9")
  (loop [board initial-board
         player-x-turn? true]
    (let [print-the-board (print-board board)]
      (cond
        (winner? board (player-turn (not player-x-turn?))) (str "Congratulations you have won the game!")
        (tie? board) (str "It is a tie!")
        :else
        (recur
          (move board (take-turn player-x-turn?))
          (not player-x-turn?))))))

(defn -main
  [& args]
  (start-game))

