(ns undirected.minimum-spanning-tree-test
  (:require [clojure.test :refer :all]
            [undirected.edge :refer [edge]]
            [undirected.graph :as g :refer [graph]]
            [undirected.weighted-graph :refer [total-weight weighted-graph]]
            [vertices :refer :all])
  (:use [undirected.minimum-spanning-tree]))

(deftest spanning-tree?-test
  (testing "Несвязный граф"
    (is (thrown? AssertionError (spanning-tree? (graph [A B]) (graph [A])))))
  (testing "n = 1"
    (is (spanning-tree? (graph [A]) (graph [A]))))
  (testing "n = 2"
    (is (spanning-tree? (graph [A B] [A B]) (graph [A B] [A B])))
    (is (not (spanning-tree? (graph [A B] [A B]) (graph [A B])))))
  (testing "n = 3"
    (let [g (g/complete-graph [A B C])]
      (is (spanning-tree? g (graph [A B C] [A B] [B C])))
      (is (not (spanning-tree? g (graph [A B] [A B]))))
      (is (not (spanning-tree? g (graph [A B C] [A B] [B C] [C A]))))))
  (testing "n = 3, already a tree"
    (let [g (graph [A B C] [A B] [B C])]
      (is (spanning-tree? g (graph [A B C] [A B] [B C])))
      (is (not (spanning-tree? g (graph [A B C] [A B] [A C]))))))
  (testing "n = 4"
    (let [g (g/complete-graph [A B C D])]
      (is (spanning-tree? g (graph [A B C D] [A B] [B C] [C D])))
      (is (not (spanning-tree? g (graph [A B C D] [A B] [C D]))))
      (is (not (spanning-tree? g (graph [A B C D] [A B] [B C] [C A]))))
      (is (not (spanning-tree? g (graph [A B C] [A B] [B C])))))))

(deftest edge-incident-some-edge?-test
  (is (edge-incident-some-edge? (graph [A B] [A B]) (edge A C)))
  (is (edge-incident-some-edge? (graph [A B] [A B]) (edge B C)))
  (is (not (edge-incident-some-edge? (graph [A B] [A B]) (edge C D))))
  (is (not (edge-incident-some-edge? (graph [A B C] [A B]) (edge C D))))
  (is (thrown? AssertionError (edge-incident-some-edge? (graph [A B] [A B]) (edge A B)))))

(deftest edge-creates-cycle?-test
  (is (edge-creates-cycle? (graph [A B C] [A B] [B C]) (edge A C)))
  (is (edge-creates-cycle? (graph [A B C D] [A B] [B C] [C D]) (edge A D)))
  (is (not (edge-creates-cycle? (graph [A]) (edge A B))))
  (is (not (edge-creates-cycle? (graph [A B]) (edge A B))))
  (is (not (edge-creates-cycle? (graph [A B C] [A B]) (edge B C))))
  (is (thrown? AssertionError (edge-creates-cycle? (graph [A B] [A B]) (edge A B)))))

(deftest minimum-spanning-tree-prim-1-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 1] [C A 2])]
    (is (= (weighted-graph [A B C] [A B 1] [B C 1])
           (minimum-spanning-tree-prim g)))))

(deftest minimum-spanning-tree-prim-2-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 2] [C A 1])]
    (is (= (weighted-graph [A B C] [A B 1] [A C 1])
           (minimum-spanning-tree-prim g)))))

(deftest minimum-spanning-tree-kruskal-1-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 1] [C A 2])]
    (is (= (weighted-graph [A B C] [A B 1] [B C 1])
           (minimum-spanning-tree-kruskal g)))))

(deftest minimum-spanning-tree-kruskal-2-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 2] [C A 1])]
    (is (= (weighted-graph [A B C] [A B 1] [A C 1])
           (minimum-spanning-tree-kruskal g)))))

(def example-graph
  (weighted-graph [A B C D E F G H I]
                  [A B 1] [A D 3] [A E 2]
                  [B C 4] [B D 2] [B E 2] [B F 3]
                  [C E 2]
                  [D E 1] [D G 3]
                  [E F 3] [E G 2] [E H 2] [E I 3]
                  [F H 5] [F I 1]
                  [G H 1]
                  [H I 3]))

(deftest minimum-spanning-tree-prim-example-test
  (let [t (minimum-spanning-tree-prim example-graph)]
    (is (spanning-tree? example-graph t))
    (is (= 13 (total-weight t)))))

(deftest minimum-spanning-tree-kruskal-example-test
  (let [t (minimum-spanning-tree-kruskal example-graph)]
    (is (spanning-tree? example-graph t))
    (is (= 13 (total-weight t)))))
