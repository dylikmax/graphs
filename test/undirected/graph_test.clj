(ns undirected.graph-test
  (:require [clojure.test :refer :all]
            [undirected.edge :refer [edge]]
            [vertices :refer :all])
  (:use [undirected.graph]))

(deftest make-graph-empty-test
  (let [g (graph [])]
    (is (empty? (vertices g)))
    (is (empty? (edges g)))))

(deftest make-graph-one-test
  (let [g (graph [A])]
    (is (= #{A} (vertices g)))
    (is (empty? (edges g)))))

(deftest make-graph-two-test
  (let [g (graph [A B] [A B])]
    (is (= #{A B} (vertices g)))
    (is (= #{(edge A B)} (edges g)))))

(deftest make-graph-unique-vertices-test
  (is (thrown? AssertionError (graph [A A A]))))

(deftest make-graph-unique-edges-test
  (is (thrown? AssertionError (graph [A B] [A B] [A B]))))

(deftest make-graph-edges-match-vertices-test
  (is (thrown? AssertionError (graph [A B C] [C D]))))

(deftest contains-vertex?-test
  (let [g (graph [A B] [A B])]
    (is (contains-vertex? g A))
    (is (contains-vertex? g B))
    (is (not (contains-vertex? g C)))))

(deftest contains-edge?-test
  (let [g (graph [A B] [A B])]
    (is (contains-edge? g (edge A B)))
    (is (contains-edge? g (edge B A)))
    (is (not (contains-edge? g (edge A C))))
    (is (not (contains-edge? g (edge C D))))))

(deftest order-test
  (is (= 0 (order (graph []))))
  (is (= 1 (order (graph [A]))))
  (is (= 2 (order (graph [A B] [A B])))))

(deftest edge-count-test
  (is (= 0 (edge-count (graph []))))
  (is (= 0 (edge-count (graph [A]))))
  (is (= 1 (edge-count (graph [A B] [A B]))))
  (is (= 2 (edge-count (graph [A B C] [A B] [B C])))))

(deftest incident-edges-test
  (let [g (graph [A B C D] [A B] [B C])]
    (is (= #{(edge A B)} (incident-edges g A)))
    (is (= #{(edge A B) (edge B C)} (incident-edges g B)))
    (is (= #{(edge B C)} (incident-edges g C)))
    (is (= #{} (incident-edges g D)))
    (is (thrown? AssertionError (incident-edges g E)))))

(deftest adjacent-vertices-test
  (let [g (graph [A B C D] [A B] [B C])]
    (is (= #{B} (adjacent-vertices g A)))
    (is (= #{A C} (adjacent-vertices g B)))
    (is (= #{B} (adjacent-vertices g C)))
    (is (= #{} (adjacent-vertices g D)))
    (is (thrown? AssertionError (adjacent-vertices g E)))))

(deftest adjacent-test
  (let [g (graph [A B C] [A B] [B C])]
    (is (adjacent? g A B))
    (is (adjacent? g B A))
    (is (adjacent? g B C))
    (is (adjacent? g C B))
    (is (not (adjacent? g A A)))
    (is (not (adjacent? g A C)))
    (is (not (adjacent? g C A)))
    (is (thrown? AssertionError (adjacent? g A D)))
    (is (thrown? AssertionError (adjacent? g E B)))))

(deftest degree-test
  (let [g (graph [A B C D] [A B] [B C])]
    (is (= 1 (degree g A)))
    (is (= 2 (degree g B)))
    (is (= 1 (degree g C)))
    (is (= 0 (degree g D)))
    (is (thrown? AssertionError (degree g E)))))

(deftest degrees-test
  (let [g (graph [A B C D] [A B] [B C])]
    (is (= [0 1 1 2] (sort (degrees g))))))

(deftest pendant-test
  (let [g (graph [A B C D] [A B] [B C])]
    (is (pendant? g A))
    (is (not (pendant? g B)))
    (is (pendant? g C))
    (is (not (pendant? g D)))
    (is (thrown? AssertionError (pendant? g E)))))

(deftest isolated-test
  (let [g (graph [A B C D] [A B] [B C])]
    (is (not (isolated? g A)))
    (is (not (isolated? g B)))
    (is (not (isolated? g C)))
    (is (isolated? g D))
    (is (thrown? AssertionError (isolated? g E)))))

(deftest sum-degrees-equals-double-edge-count-test
  (let [g (graph [A B C D] [A B] [B C])]
    (is (= (apply + (degrees g))
           (* 2 (edge-count g))))))

(deftest empty-graph?-test
  (is (empty-graph? (graph [])))
  (is (empty-graph? (graph [A])))
  (is (empty-graph? (graph [A B])))
  (is (not (empty-graph? (graph [A B] [A B])))))

(deftest empty-graph-test
  (is (= (graph []) (empty-graph [])))
  (is (= (graph [A]) (empty-graph [A])))
  (is (= (graph [A B]) (empty-graph [A B]))))

(deftest complete-graph?-test
  (is (complete-graph? (graph [])))
  (is (complete-graph? (graph [A])))
  (is (complete-graph? (graph [A B] [A B])))
  (is (not (complete-graph? (graph [A B C] [A B]))))
  (is (not (complete-graph? (graph [A B C] [A B] [B C]))))
  (is (complete-graph? (graph [A B C] [A B] [B C] [C A]))))

(deftest complete-graph-test
  (is (= (graph []) (complete-graph [])))
  (is (= (graph [A]) (complete-graph [A])))
  (is (= (graph [A B] [A B]) (complete-graph [A B])))
  (is (= (graph [A B C] [A B] [A C] [B C]) (complete-graph [A B C]))))

(deftest one-edge-graph-test
  (is (= (graph [A B] [A B])
         (one-edge-graph (edge A B)))))

(deftest subgraph?-test
  (let [g1 (graph [A B] [A B])
        g2 (graph [A B C D] [A B] [C D])
        g3 (graph [A B C D] [A B] [B C] [C D])]
    (is (subgraph? g1 g1))
    (is (subgraph? g1 g2))
    (is (subgraph? g2 g3))

    (is (not (subgraph? g2 g1)))
    (is (not (subgraph? g3 g2)))))

(deftest union-test
  (is (= (graph [A B C] [A B] [B C])
         (union (graph [A B] [A B])
                (graph [B C] [B C])))))

(deftest intersection-test
  (is (= (graph [B C] [B C])
         (intersection (graph [A B C] [A B] [B C])
                       (graph [B C D] [B C] [C D])))))

(deftest difference-test
  (is (= (graph [A B C] [A B])
         (difference (graph [A B C] [A B] [B C])
                     (graph [B C D] [B C] [C D])))))

(deftest example-of-subgraphs-union-intersection-and-difference
  ;;   1 - 2            1 - 2     2         1     1 - 2
  ;;   | / |            | /       |         |       /
  ;;   3 - 4 - 5        3 - 4     4 - 5     3     3 - 4
  ;;     G                G1        G2      G3      G4
  (let [g (graph [1 2 3 4 5] [1 2] [1 3] [2 3] [2 4] [3 4] [4 5])
        g1 (graph [1 2 3 4] [1 2] [1 3] [2 3] [3 4])
        g2 (graph [2 4 5] [2 4] [4 5])
        g3 (graph [1 3] [1 3])
        g4 (graph [1 2 3 4] [1 2] [2 3] [3 4])]
    (is (= g (union g1 g2)))
    (is (= g3 (intersection g1 g3)))
    (is (= (graph []) (intersection g2 g3)))
    (is (= g4 (difference g1 g3)))))

(deftest add-edge-test
  (is (= (graph [A B C] [A B] [B C])
         (add-edge (graph [A B] [A B]) (edge B C)))))

(deftest remove-edge-test
  (is (= (graph [A B C] [A B])
         (remove-edge (graph [A B C] [A B] [B C]) (edge B C)))))

(deftest disjoint-test
  (is (disjoint? (graph [A B] [A B]) (graph [C D] [C D])))
  (is (not (disjoint? (graph [A B] [A B]) (graph [B C] [B C])))))

(deftest mutually-disjoint-test
  (is (mutually-disjoint? [(graph [A B] [A B])
                           (graph [C D] [C D])
                           (graph [E])]))
  (is (not (mutually-disjoint? [(graph [A B] [A B])
                                (graph [C D] [C D])
                                (graph [A])]))))
