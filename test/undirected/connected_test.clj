(ns undirected.connected-test
  (:require [clojure.test :refer :all]
            [vertices :refer :all]
            [undirected.graph :refer [graph]])
  (:use undirected.connected))

(deftest connected-component-test
  (is (= (graph [A])
         (connected-component (graph [A]) A)))
  (is (= (graph [B])
         (connected-component (graph [A B]) B)))
  (is (= (graph [A B] [A B])
         (connected-component (graph [A B C] [A B]) A)))
  (is (= (graph [C])
         (connected-component (graph [A B C] [A B]) C))))

(deftest connected-components-test
  (is (= []
         (connected-components (graph []))))
  (is (= [(graph [A])]
         (connected-components (graph [A]))))
  (is (= [(graph [A]) (graph [B])]
         (connected-components (graph [A B]))))
  (is (= [(graph [A B] [A B]) (graph [C])]
         (connected-components (graph [A B C] [A B])))))

(deftest connected-vertices?-test
  (let [g (graph [A B C D] [A B] [C D])]
    (is (connected-vertices? g A B))
    (is (connected-vertices? g B A))
    (is (connected-vertices? g C D))
    (is (not (connected-vertices? g A C)))
    (is (not (connected-vertices? g A D)))))

(deftest connected?-test
  (is (connected? (graph [])))
  (is (connected? (graph [A])))
  (is (connected? (graph [A B] [A B])))
  (is (connected? (graph [A B C D] [A B] [B C] [C D])))
  (is (not (connected? (graph [A B]))))
  (is (not (connected? (graph [A B C D] [A B] [C D])))))
