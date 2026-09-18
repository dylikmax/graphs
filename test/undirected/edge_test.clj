(ns undirected.edge-test
  (:require [clojure.test :refer [deftest is]]
            [vertices :refer :all])
  (:use undirected.edge))

(deftest make-edge-test
  (is (= #{1 2} (ends (make-edge [1 2]))))
  (is (thrown? AssertionError (make-edge [1 1]))))

(deftest edge-test
  (is (= #{1 2} (ends (edge 1 2))))
  (is (thrown? AssertionError (edge 1 1))))

(deftest incident-test
  (is (incident? (edge A B) A))
  (is (incident? (edge A B) B))
  (is (not (incident? (edge A B) C))))

(deftest edges-incident-test
  (is (edges-incident? (edge A B) (edge A B)))
  (is (edges-incident? (edge A B) (edge B C)))
  (is (not (edges-incident? (edge A B) (edge C D)))))

(deftest other-end-test
  (let [e (edge 1 2)]
    (is (= 2 (other-end e 1)))
    (is (= 1 (other-end e 2)))
    (is (thrown? AssertionError (other-end e 3)))))
