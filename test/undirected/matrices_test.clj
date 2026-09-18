(ns undirected.matrices-test
  (:require [clojure.test :refer :all])
  (:use undirected.graph
        undirected.matrices))

(deftest adjacency-matrix-test
  (let [g (graph [1 2 3 4 5] [1 2] [1 3] [2 3] [3 4])]
    (is (= [[0 1 1 0 0]
            [1 0 1 0 0]
            [1 1 0 1 0]
            [0 0 1 0 0]
            [0 0 0 0 0]]
           (adjacency-matrix g)))))

(deftest incidence-matrix-test
  (let [g (graph [1 2 3 4 5] [1 2] [1 3] [2 3] [3 4])]
    (is (= [[1 1 0 0 0]
            [1 0 1 0 0]
            [0 1 1 0 0]
            [0 0 1 1 0]]
           (incidence-matrix g)))))
