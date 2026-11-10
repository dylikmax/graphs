(ns directed.route-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc]]
            [directed.graph :refer [graph]]
            [vertices :refer :all])
  (:use [directed.route]))

(deftest make-route-test
  (let [r (route A B C A B)]
    (is (= [A B C A B] (vertices r)))
    (is (= [(arc A B) (arc B C) (arc C A) (arc A B)] (arcs r)))
    (is (= 4 (length r)))
    (is (= A (start r)))
    (is (= B (end r)))))

(deftest make-route-zero-edges-test
  (let [r (route A)]
    (is (= [A] (vertices r)))
    (is (= [] (arcs r)))
    (is (= 0 (length r)))
    (is (= A (start r)))
    (is (= A (end r)))))

(deftest make-route-different-vertices-test
  (is (thrown? AssertionError (route A B B C))))

(deftest route->graph-test
  (is (= (graph [A B C] [A B] [B C])
         (route->graph (route A B C))))
  (is (= (graph [A B C] [A B] [B C] [C B] [B A])
         (route->graph (route A B C B A))))
  (is (= (graph [A B C] [A B] [B C] [C A])
         (route->graph (route A B C A B)))))

(deftest graph-contains-route-test
  (let [g (graph [A B C D] [A B] [B C] [C A])]
    (is (graph-contains-route? g (route A B C)))
    (is (graph-contains-route? g (route A B C A B)))
    (is (not (graph-contains-route? g (route A D))))))

(deftest chain?-test
  (is (chain? (route A)))
  (is (chain? (route A B C)))
  (is (chain? (route A B C A)))
  (is (chain? (route A B C D B E)))
  (is (chain? (route A B A)))
  (is (chain? (route A B C B A)))
  (is (not (chain? (route A B C A B)))))

(deftest path?-test
  (is (path? (route A)))
  (is (path? (route A B C)))
  (is (path? (route A B C A)))
  (is (path? (route A B A)))
  (is (not (path? (route A B C B A))))
  (is (not (path? (route A B C D B))))
  (is (not (path? (route A B C D B E)))))

(deftest cyclic?-test
  (is (cyclic? (route A)))
  (is (cyclic? (route A B A)))
  (is (cyclic? (route A B C A)))
  (is (cyclic? (route A B C A B C A)))
  (is (not (cyclic? (route A B C)))))

(deftest cycle?-test
  (is (cycle? (route A)))
  (is (cycle? (route A B C A)))
  (is (cycle? (route A B C D B E A)))
  (is (cycle? (route A B A)))
  (is (cycle? (route A B C B A)))
  (is (not (cycle? (route A B C A B C A))))
  (is (not (cycle? (route A B C)))))

(deftest circuit?-test
  (is (circuit? (route A)))
  (is (circuit? (route A B C A)))
  (is (circuit? (route A B A)))
  (is (not (circuit? (route A B C B A))))
  (is (not (circuit? (route A B C D B))))
  (is (not (circuit? (route A B C D B E A)))))
