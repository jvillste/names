(ns names.dimensional-space
  (:import [names.space Space])
  (:use clojure.test))

(defn coodinates [index dimension-sizes])

(deftest coodinates-test
  (is (= (map #(coodinates % [2 2 2])
              (range 8))
         [[0 0 0] [1 0 0] [0 1 0] [0 0 1]
          [1 1 0] [1 0 1] [0 1 1] [1 1 1]])))

(run-tests)