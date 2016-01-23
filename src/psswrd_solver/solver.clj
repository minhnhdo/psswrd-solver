(ns psswrd-solver.solver
  (:require [psswrd-solver.solutions :as solutions]
            [psswrd-solver.concatenation
             :refer [make]
             :rename {make make-concatenation}]
            [psswrd-solver.permutation
             :refer [make]
             :rename {make make-permutation}]))

(defrecord Solver [concatenation permutation]
  solutions/Solution
  (current [this]
    (solutions/current permutation))
  (next [this]
    (let [new-permutation (solutions/next permutation)]
      (if (not (nil? new-permutation))
        (Solver. concatenation new-permutation)
        (let [new-concatenation (solutions/next concatenation)]
          (if (not (nil? new-concatenation))
            (Solver. new-concatenation
                     (make-permutation (solutions/current new-concatenation)))
            nil))))))

(defn make [spec]
  (let [concatenation (make-concatenation spec)]
    (Solver. concatenation
             (make-permutation (solutions/current concatenation)))))
