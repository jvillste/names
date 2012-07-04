(ns names.linear-space
  (:require [names.space :as space])
  (:use clojure.test))

(defn size [dimension-sizes]
  (reduce * dimension-sizes))

(deftest size-test
  (is (= (size [2 3])
         6)))

(defn coordinates [index dimension-sizes]
  (loop [digits []
         index index
         dimension-sizes dimension-sizes]
    (if (seq dimension-sizes)
      (recur (conj digits (rem index (first dimension-sizes)))
             (quot index (first dimension-sizes))
             (rest dimension-sizes))
      digits)))

(deftest coordinates-test
  (is (= (map #(coordinates % [2 2])
              (range 5))
         [[0 0] [1 0]
          [0 1] [1 1] [0 0]]))

  (is (= (map #(coordinates % [3 2])
              (range 6))
         [[0 0] [1 0] [2 0] [0 1] [1 1] [2 1]])))


(defn index [coordinates dimension-sizes]
  (loop [result 0
         multiplier 1
         coordinates coordinates
         dimension-sizes dimension-sizes]
    (if (seq coordinates)
      (recur (+ result
                (* multiplier
                   (first coordinates)))
             (* multiplier
                (first dimension-sizes))
             (rest coordinates)
             (rest dimension-sizes))
      result)))

(deftest index-test
  (is (= (index [1 0] [2 2])
         1))

  (is (= (index [1 1] [3 2])
         4)))


(defrecord LinearSpace [dimension-sizes])

(extend-protocol space/Space
  LinearSpace
  (size [linear-space] (size (:dimension-sizes linear-space)))
  (coordinates [linear-space index] (coordinates index (:dimension-sizes linear-space)))
  (index [linear-space coordinates] (index coordinates (:dimension-sizes linear-space))))

(run-tests)