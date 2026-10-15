(ns undirected.tree-test
  (:require [clojure.test :refer :all]
            [undirected.graph :refer [graph]]
            [vertices :refer :all])
  (:use [undirected.tree]))

(deftest edge-count-tree?-test
  (is (edge-count-tree? (graph [A])))
  (is (edge-count-tree? (graph [A B] [A B])))
  (is (edge-count-tree? (graph [A B C D] [A B] [B C] [C A])))
  (is (not (edge-count-tree? (graph [A B]))))
  (is (not (edge-count-tree? (graph [A B C] [A B] [B C] [C A])))))

(deftest tree?-test
  (is (tree? (graph [A])))
  (is (tree? (graph [A B] [A B])))
  (is (not (tree? (graph [A B]))))
  (is (not (tree? (graph [A B C] [A B] [B C] [C A])))))

(deftest forest?-test
  (is (forest? (graph [])))
  (is (forest? (graph [A])))
  (is (forest? (graph [A B])))
  (is (forest? (graph [A B] [A B])))
  (is (forest? (graph [A B C D] [A B] [C D])))
  (is (not (forest? (graph [A B C] [A B] [B C] [C A]))))
  (is (not (forest? (graph [A B C D] [A B] [B C] [C A])))))
