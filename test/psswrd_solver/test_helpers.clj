(ns psswrd-solver.test-helpers
  (:require [psswrd-solver.solutions :as solutions]
            [clojure.test :refer :all]))

(def alphabet "0123456789abcdefghijklmnopqrstuvwxyz")

(defn count-solutions [solver]
  (loop [s solver
         acc 0]
    (if (nil? s)
      acc
      (recur (solutions/next s) (+ acc 1)))))

(defn fact [number]
  (loop [acc 1
         n number]
    (if (<= n 1)
      acc
      (recur (* acc n) (- n 1)))))

(deftest fact-1
  (is (= 1 (fact 1))))

(deftest fact-2
  (is (= 2 (fact 2))))

(deftest fact-3
  (is (= 6 (fact 3))))

(deftest fact-4
  (is (= 24 (fact 4))))

(deftest fact-5
  (is (= 120 (fact 5))))

(deftest fact-6
  (is (= 720 (fact 6))))

(deftest fact-7
  (is (= 5040 (fact 7))))

(deftest fact-8
  (is (= 40320 (fact 8))))
