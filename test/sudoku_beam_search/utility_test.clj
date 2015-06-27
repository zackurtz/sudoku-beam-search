(ns sudoku-beam-search.utility-test
  (:require [clojure.test :refer :all]
            [sudoku-beam-search.utility :refer :all]))

(def empty-loc-test [[1 '- '- '-]
					   ['- '- '- '-]
					   ['- '- '- '-]
					   ['- '- '- '-]])


(deftest test-empty-cell
  (testing "empty-cell rejection"
    (is (= false (empty-cell 1))))
  (testing "empty-cell detection"
  	(is (= true (empty-cell '-)))))

(deftest test-empty-loc
	(testing "empty-loc detection"
		(is (= true (empty-loc empty-loc-test 1 1))))
	(testing "empty-loc rejection"
		(is (= false (empty-loc empty-loc-test 0 0))))
	(testing "empty-loc out of range"
		(is (= false (empty-loc empty-loc-test -1 5)))))