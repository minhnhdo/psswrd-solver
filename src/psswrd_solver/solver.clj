(ns psswrd-solver.solver
  (:require [psswrd-solver.solutions :as solutions]
            [psswrd-solver.selection
             :refer [make]
             :rename {make make-selection}]
            [psswrd-solver.permutation
             :refer [make]
             :rename {make make-permutation}]))

(defrecord Solver [transformed-spec selections permutation]
  solutions/Solution
  (current [this]
    (solutions/current permutation))
  (next [this]
    (let [new-permutation (solutions/next permutation)]
      (if (not (nil? new-permutation))
        (Solver. transformed-spec selections new-permutation)
        (let [unfilled-selections (loop [unfilled-selections selections]
                                    (let [s (solutions/next (last unfilled-selections))]
                                      (cond
                                        (not (nil? s)) (conj (pop unfilled-selections) s)
                                        (zero? (count unfilled-selections)) nil
                                        :else (recur (pop unfilled-selections)))))]
          (if (nil? unfilled-selections)
            nil
            (let [new-selections (->> (drop (count unfilled-selections)
                                            transformed-spec)
                                      (map #(apply make-selection %))
                                      (into unfilled-selections))]
              (Solver. transformed-spec
                       new-selections
                       (make-permutation (apply str
                                                (map solutions/current
                                                     new-selections)))))))))))

(defn make [spec]
  (let [transformed-spec (vec (map #(vector (:guess %)
                                            (+ (:number-of-gold %)
                                               (:number-of-silver %)))
                                   spec))
        selections (vec (map #(apply make-selection %) transformed-spec))]
    (Solver. transformed-spec
             selections
             (make-permutation (apply str (map solutions/current selections))))))
