(ns psswrd-solver.core
  (:require [clj-webdriver.taxi :as taxi]
            [psswrd-solver.solutions :as solutions]
            [psswrd-solver.solver :refer [make] :rename {make make-solver}])
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
  (println (str "    submitting guess '" guess "'"))
  (taxi/input-text "input.input_text" guess)
  (taxi/submit "input.buttonSmall"))

(defn sweep [characters code-length]
  (let [length (.length characters)
        times (quot length code-length)
        incomplete-spec (for [i (range times)]
                          (let [guess (.substring characters
                                                  (* i code-length)
                                                  (* (inc i) code-length))]

                            (submit-guess guess)
                            (parse-latest-hint)))
        remaining-count (- code-length (apply + (map #(+ (:number-of-gold %)
                                                         (:number-of-silver %))
                                                     incomplete-spec)))
        remaining-characters (.substring characters (* times code-length))]
    (filter #(not (zero? (+ (:number-of-gold %) (:number-of-silver %))))
            (conj incomplete-spec {:guess remaining-characters
                                   :number-of-gold 0
                                   :number-of-silver remaining-count}))))

(defn make-filter [{:keys [^String guess number-of-gold number-of-silver]}]
  (println (str "    making filter for guess '" guess
                "' ngold=" number-of-gold
                " nsilver=" number-of-silver))
  (fn [^String code]
    (loop [i 0
           correct-places 0
           correct-digits 0]
      (if (= i (.length code))
        (and (= correct-digits number-of-silver)
             (= correct-places number-of-gold))
        (cond
          (= (.charAt code i) (.charAt guess i)) (recur (inc i)
                                                        (inc correct-places)
                                                        correct-digits)
          (> (.indexOf guess (int (.charAt code i))) -1) (recur (inc i)
                                                                correct-places
                                                                (inc correct-digits))
          :else (recur (inc i) correct-places correct-digits))))))

(defn different-characters [s1 s2]
  (apply str (map #(if (> (.indexOf s1 (int %)) -1) nil %) s2)))

(defn remove-impossible-characters [characters impossible-characters]
  (apply str (filter #(not (impossible-characters %)) characters)))

(defn new-impossible-characters-and-spec
  [impossible-characters current-spec new-characters]
  (println (str "    adding '" new-characters "' to impossible characters"))
  (let [new-impossible-characters (into impossible-characters new-characters)]
    [new-impossible-characters
     (->> current-spec
          (map #(assoc %
                       :guess
                       (remove-impossible-characters (:guess %)
                                                     new-impossible-characters)))
          (filter #(not (zero? (+ (:number-of-gold %)
                                  (:number-of-silver %)))))
          vec)]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (taxi/set-driver! {:browser :firefox})
  (taxi/to "http://walisu.com/psswrd")
  ;; let the game begin!
  (taxi/submit "input.button[value=\"START!\"]")
  (loop []
    ;; parse level information
    (let [problem-statement (taxi/text "body > div")
          level (Integer/parseInt (.substring problem-statement
                                              (inc (.indexOf problem-statement (int \space)))
                                              (.indexOf problem-statement (int \:)))
                                  10)
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
                          (.substring problem-statement))
          ;; start solving it by first sweeping over the alphabet
          spec (do (println (str "### Starting level " level
                                 " with code length " code-length
                                 " and alphabet '" characters "'"))
                   (sweep characters code-length))]
      (loop [impossible-characters #{}
             filters (map make-filter
                          (filter #(= code-length (.length (:guess %))) spec))
             current-spec spec
             solver (make-solver spec)
             last-hint (last spec)]
        (let [combined-filters (apply every-pred filters)
              current-solver (loop [current-solver solver]
                               (cond
                                 (nil? current-solver) nil
                                 (combined-filters (solutions/current current-solver)) current-solver
                                 :else (recur (solutions/next current-solver))))]
          (submit-guess (solutions/current current-solver))
          (if (and (not (nil? (taxi/element "form > h1 > span")))
                   (= "Correct!" (taxi/text "form > h1 > span")))
            (taxi/submit "input.button[value=\"NEXT LEVEL\"]")
            (let [latest-hint (parse-latest-hint)
                  guess-count (+ (:number-of-gold latest-hint)
                                 (:number-of-silver latest-hint))]
              (cond
                (= code-length guess-count) (let [new-spec [latest-hint]]
                                              (println (str "    matched all characters, changing spec to " new-spec))
                                              (recur impossible-characters
                                                     (conj filters (make-filter latest-hint))
                                                     new-spec
                                                     (make-solver new-spec)
                                                     latest-hint))
                (zero? guess-count) (let [[new-impossible-characters new-spec] (new-impossible-characters-and-spec impossible-characters spec (:guess latest-hint))]
                                      (recur new-impossible-characters
                                             filters
                                             new-spec
                                             (make-solver new-spec)
                                             latest-hint))
                (>= guess-count (- code-length 2)) (let [[new-impossible-characters new-spec] (new-impossible-characters-and-spec impossible-characters
                                                                                                                                  [latest-hint {:guess (remove-impossible-characters characters
                                                                                                                                                                                     (into #{} (:guess latest-hint)))
                                                                                                                                                :number-of-gold 0
                                                                                                                                                :number-of-silver (- code-length guess-count)}]
                                                                                                                                  "")]
                                                     (recur new-impossible-characters
                                                            (conj filters (make-filter latest-hint))
                                                            new-spec
                                                            (make-solver new-spec)
                                                            latest-hint))
                (and (= (.length (different-characters (:guess last-hint)
                                                       (:guess latest-hint)))
                        (- (+ (:number-of-gold last-hint)
                              (:number-of-silver last-hint))
                           guess-count))
                     (not (zero? (- (+ (:number-of-gold last-hint)
                                       (:number-of-silver last-hint))
                                    guess-count))))
                (let [[new-impossible-characters new-spec] (new-impossible-characters-and-spec impossible-characters spec (different-characters (:guess last-hint) (:guess latest-hint)))]
                  (recur new-impossible-characters
                         (conj filters (make-filter latest-hint))
                         new-spec
                         (make-solver new-spec)
                         latest-hint))
                (and (= (.length (different-characters (:guess latest-hint)
                                                       (:guess last-hint)))
                        (- guess-count
                           (+ (:number-of-gold last-hint)
                              (:number-of-silver last-hint))))
                     (not (zero? (- guess-count
                                    (+ (:number-of-gold last-hint)
                                       (:number-of-silver last-hint))))))
                (let [[new-impossible-characters new-spec] (new-impossible-characters-and-spec impossible-characters spec (different-characters (:guess latest-hint) (:guess last-hint)))]
                  (recur new-impossible-characters
                         (conj filters (make-filter latest-hint))
                         new-spec
                         (make-solver new-spec)
                         latest-hint))
                :else (recur impossible-characters
                             (conj filters (make-filter latest-hint))
                             current-spec
                             current-solver
                             latest-hint)))))))
    (recur))
  (taxi/quit))
