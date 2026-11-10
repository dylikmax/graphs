(ns directed.graph-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc]]
            [vertices :refer :all])
  (:use [directed.graph]))

(deftest make-graph-empty-test
  (let [g (graph [])]
    (is (empty? (vertices g)))
    (is (empty? (arcs g)))))

(deftest make-graph-one-test
  (let [g (graph [A])]
    (is (= #{A} (vertices g)))
    (is (empty? (arcs g)))))

(deftest make-graph-two-test
  (let [g (graph [A B] [A B])]
    (is (= #{A B} (vertices g)))
    (is (= #{(arc A B)} (arcs g)))))

(deftest make-graph-unique-vertices-test
  (is (thrown? AssertionError (graph [A A A]))))

(deftest make-graph-unique-arcs-test
  (is (thrown? AssertionError (graph [A B] [A B] [A B]))))

(deftest make-graph-arcs-match-vertices-test
  (is (thrown? AssertionError (graph [A B C] [C D]))))

(deftest contains-vertex?-test
  (let [g (graph [A B] [A B])]
    (is (contains-vertex? g A))
    (is (contains-vertex? g B))
    (is (not (contains-vertex? g C)))))

(deftest contains-arc?-test
  (let [g (graph [A B] [A B])]
    (is (contains-arc? g (arc A B)))
    (is (not (contains-arc? g (arc B A))))
    (is (not (contains-arc? g (arc A C))))
    (is (not (contains-arc? g (arc C D))))))

(deftest neighbours-test
  (let [g (graph [A B C D] [A B] [A C] [B C])]
    (is (= #{B C} (neighbours g A)))
    (is (= #{C} (neighbours g B)))
    (is (= #{} (neighbours g C)))
    (is (= #{} (neighbours g D)))
    (is (thrown? AssertionError (neighbours g E)))))

(deftest subgraph?-test
  (is (subgraph? (graph [A]) (graph [A])))
  (is (subgraph? (graph [A]) (graph [A B])))
  (is (subgraph? (graph [A B] [A B]) (graph [A B C] [A B] [B C])))
  (is (not (subgraph? (graph [A B]) (graph [A]))))
  (is (not (subgraph? (graph [A B] [A B]) (graph [A B C])))))
