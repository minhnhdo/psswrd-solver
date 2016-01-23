(ns psswrd-solver.concatenation
  (:require [psswrd-solver.solutions :as solutions]
            [psswrd-solver.selection
             :refer [make]
             :rename {make make-selection}]))

(defrecord Concatenation [transformed-spec selections]
  solutions/Solution
  (current [this]
    (apply str (map solutions/current selections)))
  (next [this]
    (let [unfilled-selections (loop [unfilled-selections selections]
                                (let [s (solutions/next (last unfilled-selections))]
                                  (cond
                                    (not (nil? s)) (conj (pop unfilled-selections) s)
                                    (zero? (count unfilled-selections)) nil
                                    :else (recur (pop unfilled-selections)))))]
      (if (nil? unfilled-selections)
        nil
        (Concatenation. transformed-spec
                        (->> (drop (count unfilled-selections)
                                   transformed-spec)
                             (map #(apply make-selection %))
                             (into unfilled-selections)))))))

(defn make [spec]
  (let [transformed-spec (vec (map #(vector (:guess %)
                                            (+ (:number-of-gold %)
                                               (:number-of-silver %)))
                                   spec))]
    (Concatenation. transformed-spec
                    (vec (map #(apply make-selection %) transformed-spec)))))
