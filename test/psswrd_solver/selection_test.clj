(ns psswrd-solver.selection-test
  (:require [clojure.test :refer :all]
            [psswrd-solver.selection :refer :all]
            [psswrd-solver.test-helpers :refer :all])
  (:import [psswrd_solver.selection SelectionOne SelectionAllButOne SelectionAll
            Selection]))

(deftest selection-make-one
  (is (instance? SelectionOne (make alphabet 1))))

(deftest selection-make-all-but-one
  (is (instance? SelectionAllButOne (make alphabet (- (.length alphabet) 1)))))

(deftest selection-make-all
  (is (instance? SelectionAll (make alphabet (.length alphabet)))))

(deftest selection-make-general
  (is (instance? Selection (make alphabet 2))))

(deftest single-selection-count
  (is (= 36 (count-solutions (make alphabet 1)))))

(deftest all-but-one-selection-count
  (is (= 36 (count-solutions (make alphabet (- (.length alphabet) 1))))))

(deftest all-selection-count
  (is (= 1 (count-solutions (make alphabet (.length alphabet))))))

(deftest code-size-2-alphabet-size-4-selection-count
  (is (= 6 ;; 4C2
         (count-solutions (make (.substring alphabet 0 4) 2)))))

(deftest code-size-3-alphabet-size-6-selection-count
  (is (= 15 ;; 6C2
         (count-solutions (make (.substring alphabet 0 6) 2)))))

(deftest code-size-5-alphabet-size-11-selection-count
  (is (= 462 ;; 11C5
         (count-solutions (make (.substring alphabet 0 11) 5)))))

(deftest code-size-2-selection-count
  (is (= 630 ;; 36C2
         (count-solutions (make alphabet 2)))))

(deftest code-size-5-selection-count
  (is (= 376992 ;; 36C5
         (count-solutions (make alphabet 5)))))
