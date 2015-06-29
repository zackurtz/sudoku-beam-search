(ns sudoku-beam-search.solver-test
  (:require [clojure.test :refer :all]
            [sudoku-beam-search.solver :refer :all]
            [sudoku-beam-search.utility :refer :all]))


(deftest test-list-valid
  (testing "test9 1,1"
    (is (= #{1 5 6 8} (list-valid (:board testc9) (:info testc9) 1 1))))
  (testing "test9 4,5"
  	(is (= #{2 5 9} (list-valid (:board testc9) (:info testc9) 4 5)))))

(deftest test-is-full
	(testing "full test"
		(is (= true (is-full solved))))
	(testing "not full test1"
		(is (= false (is-full easy-test))))
	(testing "not full test2"
		(is (= false (is-full test1))))
	(testing "not full test3"
		(is (= false (is-full test2))))
	(testing "not full test9"
		(is (= false (is-full test9)))))

(deftest test-find-moves
	(testing "solved"
		(is (= '() (find-moves solved info-4x4))))
	(testing "simple test"
		(is (= '([[3 3] #{4}]) (find-moves easy-test info-4x4)))))

(deftest test-apply-move
	(testing "simple test"
		(is (= solved (:board (first (apply-move easy-test (first (find-moves easy-test info-4x4)))))))))

(deftest test-successor-states
	(testing "simple test"
		(is (= solved (:board (first (successor-states easy-test info-4x4))))))
	(testing "test 1"
		(is (= 10 (count (successor-states test1 info-4x4))))))

(deftest test-expand-game
	(testing "simple test"
		(let [result (expand-game testc0)]
			(is (= solved (:board (first result))))
			(is (= easy-test (:board (:parent (first result))))))))

(deftest test-general-search
	(testing "easy test"
		(is (= solved (:board (first (general-search testc0 q-bfs 5 false)))))
		(is (= solved (:board (first (general-search testc0 q-bfs 5 true))))))
	(testing "test 1"
		(is (= test1-solved (:board (first (general-search testc1 q-bfs 100))))))
	(testing "test 2"
		(is (= test2-solved (:board (first (general-search testc2 q-bfs 100))))))
	(testing "test 3"
		(is (= test3-solved (:board (first (general-search testc3 q-bfs 1000))))))
	(testing "test 4"
		(is (= test4-solved (:board (first (general-search testc4 q-bfs 1000))))))
	(testing "test 5"
		(is (= test5-solved (:board (first (general-search testc5 q-bfs 1000))))))
	(testing "test 6"
		(let [results (general-search testc6 q-bfs 1000000)]
			(is (= 0 (:board (first results) (first results)))))))

	

