(ns psswrd-solver.permutation
  (:require [psswrd-solver.solutions :as solutions]))

(defn last-peek-index-of [^ints array]
  (loop [current-value -1
         index (dec (count array))]
    (if (= index -1)
      nil
      (let [v (aget ^ints array index)]
        (if (> current-value v)
          (inc index)
          (recur v (dec index)))))))

(defn index-of-larger [^ints array ^Integer start-index ^Integer elem]
  (let [length (count array)]
    (loop [i (dec length)]
      (cond
        (< i start-index) nil
        (> (aget array i) elem) i
        :else (recur (dec i))))))

(defn swap [^ints array ^Integer i ^Integer j]
  (let [tmp (aget ^ints array i)]
    (aset ^ints array i (aget ^ints array j))
    (aset ^ints array j tmp)))

(defrecord Permutation [^String alphabet ^ints state]
  solutions/Solution
  (current [this]
    (apply str (map #(.charAt alphabet %) state)))
  (next [this]
    (let [last-peek-index (last-peek-index-of state)
          length (count state)]
      (if (nil? last-peek-index)
        nil
        (do (swap state
                  (dec last-peek-index)
                  (index-of-larger state
                                   last-peek-index
                                   (aget ^ints state (dec last-peek-index))))
            (java.util.Arrays/sort ^ints state last-peek-index (count state))
            this)))))

(defn make [^String alphabet]
  (let [length (.length alphabet)
        array (int-array length)]
    (loop [i 0]
      (if (= i length)
        nil
        (do (aset ^ints array i i)
            (recur (inc i)))))
    (Permutation. alphabet array)))
