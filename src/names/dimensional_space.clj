(ns names.dimensional-space
  (:import [names.space Space])
  (:use clojure.test))

(defn size [dimension-sizes]
  (+ 1 (reduce + (map dec dimension-sizes))))


(deftest size-test
  (is (= (size [2 2 2])
         4)))

(defn coordinates [index dimension-sizes]
  (loop [coordinates []
         index (mod index (size dimension-sizes) )
         dimension-sizes dimension-sizes]
    (if (seq dimension-sizes)
      (recur (conj coordinates (if (and (> index 0)
                                        (< index (first dimension-sizes)))
                                 index
                                 0))
             (- index (- (first dimension-sizes)
                         1))
             (rest dimension-sizes))
      coordinates)))

(deftest coordinates-test
  (is (= (map #(coordinates % [2 2 2])
              (range 5))
         [[0 0 0] [1 0 0] [0 1 0] [0 0 1] [0 0 0]])))


(run-tests)