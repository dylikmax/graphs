(ns undirected.weighted-graph-test
  (:require [clojure.test :refer :all]
            [vertices :refer :all]
            [undirected.graph :as g]
            [undirected.edge :refer [edge]])
  (:use undirected.weighted-graph))

(deftest make-weighted-graph-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 3] [C A 5])]
    (is (= #{A B C} (g/vertices g)))
    (is (= #{(edge A B) (edge B C) (edge C A)} (g/edges g)))))

(deftest make-weighted-graph-weight-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 3] [C A 5])]
    (is (= {(edge A B) 1
            (edge B C) 3
            (edge C A) 5} (weights g)))
    (is (= 9 (total-weight g)))))
