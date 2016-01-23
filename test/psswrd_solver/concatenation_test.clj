(ns psswrd-solver.concatenation-test
  (:require [clojure.test :refer :all]
            [psswrd-solver.concatenation :refer :all]
            [psswrd-solver.test-helpers :refer :all]))

(deftest concatenation-test
  (is (= 48 ;; 2C1 * 4C1 * 4C2
         (count-solutions (make [{:guess "89", :number-of-gold 0, :number-of-silver 1}
                                 {:guess "0123", :number-of-gold 0, :number-of-silver 1}
                                 {:guess "4567", :number-of-gold 1, :number-of-silver 1}])))))
