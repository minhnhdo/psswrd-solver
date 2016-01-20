(ns psswrd-solver.solutions)

(defprotocol Solution
  (current [this] "Returns current solution, nil if no valid solution can be found.")
  (next [this] "Update the state for the current solver. Returns new state if successful, nil otherwise."))
