(ns psswrd-solver.solutions
  (:refer-clojure :exclude [next]))

(defprotocol Solution
  (current [this] "Returns current solution, nil if no valid solution can be found.")
  (next [this] "Update the state for the current solver. Returns new state if successful, nil otherwise."))

(extend-type nil
  Solution
  (current [this])
  (next [this]))
