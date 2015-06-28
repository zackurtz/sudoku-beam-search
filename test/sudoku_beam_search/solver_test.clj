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
		(is (= false (is-full test1))))
	(testing "not full test2"
		(is (= false (is-full test2))))
	(testing "not full test3"
		(is (= false (is-full test3))))
	(testing "not full test9"
		(is (= false (is-full test9)))))

(deftest test-find-moves
	(testing "solved"
		(is (= '() (find-moves solved info-4x4))))
	(testing "simple test"
		(is (= '([[3 3] #{4}]) (find-moves easy-test info-4x4)))))

(deftest test-apply-move
	(testing "simple test"
		(is (= solved (first (apply-move easy-test (first (find-moves easy-test info-4x4))))))))
	

