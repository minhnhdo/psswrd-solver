(ns psswrd-solver.core
  (:require [clj-webdriver.taxi :as taxi])
  (:gen-class))

(defn parse-number-of-guesses-left []
  (let [guesses-text (taxi/text ".info")]
    (Integer/parseInt (.substring guesses-text
                                  0
                                  (.indexOf guesses-text (int \space)))
                      10)))

(defn parse-latest-hint []
  (let [guess (taxi/text "table.table > tbody > tr > td")
        hint-text (taxi/text "table.table > tbody > tr > td:nth-child(3)")
        number-of-gold (Integer/parseInt (.substring hint-text
                                                     0
                                                     (.indexOf hint-text (int \space)))
                                         10)
        number-of-silver (Integer/parseInt (.substring hint-text
                                                       7
                                                       (.indexOf hint-text (int \space) 7))
                                           10)]
    {:guess guess
     :number-of-gold number-of-gold
     :number-of-silver number-of-silver}))

(defn submit-guess [guess]
  (taxi/input-text "input.input_text" guess)
  (taxi/submit "input.buttonSmall"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (taxi/set-driver! {:browser :firefox})
  (taxi/to "http://walisu.com/psswrd")
  (taxi/submit "input.button[value=\"START!\"]")
  (let [problem-statement (taxi/text "body > div")
        code-length (let [after-colon-index (+ 2 (.indexOf problem-statement (int \:)))]
                      (Integer/parseInt
                        (.substring problem-statement
                                    after-colon-index
                                    (.indexOf problem-statement
                                              (int \space)
                                              after-colon-index))
                        10))
        characters (->> (.lastIndexOf problem-statement (int \space))
                        inc
                        (.substring problem-statement))]
    (println code-length characters))
  (submit-guess "0123")
  (println (parse-latest-hint))
  (taxi/quit))
