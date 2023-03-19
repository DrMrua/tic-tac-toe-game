(ns tic-tac-toe.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-toe.core :refer :all]))
;(def initial-board [[1 2 3] [4 5 6] [7 8 9]] )
(def win-row [["X" "X" "X"] ["O" 5 6] ["O" 8 9]])
(def win-column [["X" "O" "O"] ["X" 5 6] ["X" 8 9]])
(def win-diagonal [["X" "O" 3] [4 "X" 6] ["O" 8 "X"]])

(def tie-board [["X" "O" "X"] ["O" "O" "X"] ["O" "X" "X"]])

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest initial-row
  (is (= (winner? initial-board true) nil)))

(deftest win
  (is (= (winner? initial-board (player-turn true)) nil))
  (is (= (winner? win-row (player-turn true)) true))
  (is (= (winner? win-column (player-turn true)) true))
  (is (= (winner? win-diagonal (player-turn true)) true)))

(deftest tie
  (is (= (tie? initial-board) false))
  (is (= (tie? tie-board) true)))