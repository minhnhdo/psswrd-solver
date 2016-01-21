(ns psswrd-solver.permutation-test
  (:require [clojure.test :refer :all]
            [psswrd-solver.permutation :refer :all]
            [psswrd-solver.test-helpers :refer :all]))

(deftest alphabet-size-1-permutation-count
  (is (= (fact 1) (count-solutions (make (.substring alphabet 0 1))))))

(deftest alphabet-size-2-permutation-count
  (is (= (fact 2) (count-solutions (make (.substring alphabet 0 2))))))

(deftest alphabet-size-3-permutation-count
  (is (= (fact 3) (count-solutions (make (.substring alphabet 0 3))))))

(deftest alphabet-size-4-permutation-count
  (is (= (fact 4) (count-solutions (make (.substring alphabet 0 4))))))

(deftest alphabet-size-5-permutation-count
  (is (= (fact 5) (count-solutions (make (.substring alphabet 0 4))))))

(deftest alphabet-size-6-permutation-count
  (is (= (fact 6) (count-solutions (make (.substring alphabet 0 4))))))

(deftest alphabet-size-7-permutation-count
  (is (= (fact 7) (count-solutions (make (.substring alphabet 0 4))))))

(deftest alphabet-size-8-permutation-count
  (is (= (fact 8) (count-solutions (make (.substring alphabet 0 4))))))
