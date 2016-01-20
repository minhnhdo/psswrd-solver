(ns psswrd-solver.selection-test
  (:require [clojure.test :refer :all]
            [psswrd-solver.selection :refer :all])
  (:import [psswrd_solver.selection SelectionOne SelectionAllButOne SelectionAll
            Selection]))

(def alphabet "0123456789abcdefghijklmnopqrstuvwxyz")

(deftest selection-make-one
  (is (instance? SelectionOne (make alphabet 1))))

(deftest selection-make-all-but-one
  (is (instance? SelectionAllButOne (make alphabet (- (.length alphabet) 1)))))

(deftest selection-make-all
  (is (instance? SelectionAll (make alphabet (.length alphabet)))))

(deftest selection-make-general
  (is (instance? Selection (make alphabet 2))))
