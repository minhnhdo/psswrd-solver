(ns psswrd-solver.selection
  (:require [psswrd-solver.solutions :as solutions]))

;; NOT thread-safe
(defrecord Selection [^String alphabet ^Integer code-length ^booleans state]
  solutions/Solution
  (current [this]
    (apply str (->> alphabet
                    (map vector state)
                    (filter first)
                    (map second))))
  (next [this] nil))

(defrecord SelectionAllButOne [^String alphabet ^Integer index]
  solutions/Solution
  (current [this] (str (.substring alphabet 0 index)
                       (.substring alphabet (+ index 1) (.length alphabet))))
  (next [this]
    (let [new-index (+ index 1)]
      (if (= new-index (.length alphabet))
        nil
        (SelectionAllButOne. alphabet new-index)))))

(defrecord SelectionOne [^String alphabet ^Integer index]
  solutions/Solution
  (current [this] (.charAt alphabet index))
  (next [this]
    (let [new-index (+ index 1)]
      (if (= new-index (.length alphabet))
        nil
        (SelectionOne. alphabet new-index)))))

(defrecord SelectionAll [^String alphabet]
  solutions/Solution
  (current [this] alphabet)
  (next [this] nil))

(defn make [^String alphabet ^Integer length]
  (let [alphabet-length (.length alphabet)]
    (cond
      (<= length 0) (throw (IllegalArgumentException. (str "length must be greater than zero. Got " length ".")))
      (> length alphabet-length) (throw (IllegalArgumentException. (str "length must be less than or equal to length of alphabet. Got " length ".")))
      (= length alphabet-length) (SelectionAll. alphabet)
      (= length (- alphabet-length 1)) (SelectionAllButOne. alphabet 0)
      (= length 1) (SelectionOne. alphabet 0)
      :else (let [array (boolean-array alphabet-length)]
              (java.util.Arrays/fill array 0 (- alphabet-length length) false)
              (java.util.Arrays/fill array (- alphabet-length length) alphabet-length true)
              ;; array is now [false, false, ..., false, true, true, ..., true]
              (Selection. alphabet length array)))))
