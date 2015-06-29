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

(deftest test-insert-game
	(let [old-games (list {:siblings 2} {:siblings 3})
		  complete (list {:siblings 2} {:siblings 2.5} {:siblings 3})]
		(testing "recursive"
			(is (= complete (insert-game {:siblings 2.5} old-games))))
		(testing "loop"
			(is (= complete (insert-game-loop {:siblings 2.5} old-games))))))


(deftest test-expand-game
	(testing "simple test"
		(let [result (expand-game testc0)]
			(is (= solved (:board (first result))))
			(is (= easy-test (:board (:parent (first result))))))))

(deftest test-general-search
	(testing "easy test"
		(is (= solved (:board (first (general-search testc0 q-dfs 5 false)))))
		(is (= solved (:board (first (general-search testc0 q-dfs 5 true))))))
	(testing "test 1"
		(let [results (general-search testc1 q-bestfs 1000)]
			(is (= test1-solved (:board (first results) results)))))
	(time (testing "test 2"
	 	(is (= test2-solved (:board (first (general-search testc2 q-bestfs 1000)))))))
	(time (testing "test 3"
		(is (= test3-solved (:board (first (general-search testc3 q-bestfs 1000)))))))
	(time (testing "test 4"
		(is (= test4-solved (:board (first (general-search testc4 q-bestfs 1000)))))))
	(time (testing "test 5"
	 	(let [results (general-search testc5 q-beam 1000)]
	 		(is (= test5-solved (:board (first results) (first results)))))))
	(time (testing "test 6"
		(let [results (general-search testc6 q-beam 1000)]
			(is (= test6-solved (:board (first results) results))))))
	(time (testing "test 9"
		(let [results (general-search testc9 q-beam 1000)]
			(is (= test9-solved (:board (first results) results)))))))

	

