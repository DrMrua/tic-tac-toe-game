(ns tic-tac-toe.core)

(def initial-board [[1 2 3] [4 5 6] [7 8 9]])

(defn print-board [board]
  (println (str " " (get-in board [0 0]) " | " (get-in board [0 1]) " | " (get-in board [0 2]) " "))
  (println (str "-----------"))
  (println (str " " (get-in board [1 0]) " | " (get-in board [1 1]) " | " (get-in board [1 2]) " "))
  (println (str "-----------"))
  (println (str " " (get-in board [2 0]) " | " (get-in board [2 1]) " | " (get-in board [2 2]) " ")))



;(defn read-input
;  []
;  (for [ln (line-seq (java.io.BufferedReader. *in*))]
;    (println ln)))

(defn valid?
  [input]
  (let [number (try
                 (Integer/parseInt input)
                 (catch NumberFormatException e
                   (println "Input most be an number!!")))]
    (if (contains? (set (range 1 10)) number)
      true
      false)))

(defn start
  []
  (println "You have started a new game. Press a number between 1-9")
  (loop []
    (let [input (read-line)]
      (cond
        (= input "stop") (str "game has stopped")
        (valid? input) (->> initial-board
                            (print-board )
                            (recur ))              ;move
        :else (recur)))))














