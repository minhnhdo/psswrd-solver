(ns psswrd-solver.core
  (:require [clj-webdriver.taxi :as taxi])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (taxi/set-driver! {:browser :firefox})
  (taxi/to "http://walisu.com/psswrd")
  (println (taxi/title))
  (taxi/quit))
