(ns directed.weighted-graph-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc]]
            [directed.graph :as g]
            [directed.route :refer [route]]
            [vertices :refer :all])
  (:use [directed.weighted-graph]))

(deftest make-weighted-graph-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 3] [C A 5])]
    (testing "Вершины"
      (is (= #{A B C} (g/vertices g))))
    (testing "Смежные вершины"
      (is (= #{B} (g/neighbours g A)))
      (is (= #{C} (g/neighbours g B)))
      (is (= #{A} (g/neighbours g C))))
    (testing "Веса"
      (is (= 1 (arc-length g A B)))
      (is (= 3 (arc-length g B C)))
      (is (= 5 (arc-length g C A))))))

(deftest make-weighted-graph-weight-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 3] [C A 5])]
    (is (= {(arc A B) 1
            (arc B C) 3
            (arc C A) 5} (weights g)))))

(deftest route-length-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 3] [C A 5])]
    (is (= 8 (route-length g (route B C A))))))
