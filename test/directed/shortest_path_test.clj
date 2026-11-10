(ns directed.shortest-path-test
  (:require [clojure.test :refer :all]
            [directed.route :refer [route]]
            [directed.weighted-graph :refer [route-length weighted-graph]]
            [vertices :refer :all])
  (:use [directed.shortest-path]))

(def example-graph
  (weighted-graph [S A B C D T]
                  [S A 4] [S B 7] [S C 3]
                  [A B 3] [A D 2]
                  [B A 10] [B T 2]
                  [C D 3]
                  [D T 2]))

(def example-negative-graph
  (weighted-graph [S A B T]
                  [S A 1] [S B 2]
                  [A T 3]
                  [B A -2]))

(deftest path-from-to?-test
  (is (path-from-to? example-graph S T (route S A D T)))
  (is (path-from-to? example-graph S T (route S B A D T)))
  (is (not (path-from-to? example-graph S T (route S B A B T))))
  (is (not (path-from-to? example-graph S T (route A D T))))
  (is (not (path-from-to? example-graph S T (route S A D))))
  (is (not (path-from-to? example-graph S T (route S T)))))

(deftest distance-test
  (let [path-map {B (path-info 15 A)}]
    (is (= 15 (distance path-map B)))))

(deftest distance-infinity-test
  (let [path-map {}]
    (is (= ##Inf (distance path-map B)))))

(deftest previous-test
  (let [path-map {B (path-info 15 A)}]
    (is (= A (previous path-map B)))))

(deftest previous-unknown-test
  (let [path-map {}]
    (is (= nil (previous path-map B)))))

(deftest update-distances-test
  (let [path-map-0 {S (path-info 0 nil)}
        path-map-1 (into path-map-0 (update-distances example-graph path-map-0 S [A B C]))]
    (is (= #{[A (path-info 4 S)]
             [B (path-info 7 S)]
             [C (path-info 3 S)]}
           (set (update-distances example-graph path-map-0 S [A B C]))))
    (is (= #{[D (path-info 6 C)]}
           (set (update-distances example-graph path-map-1 C [D]))))))

(deftest backtrack-path-test
  (is (= (route S)
         (backtrack-path S S {})))
  (is (= (route S T)
         (backtrack-path S T {T (path-info 10 S)})))
  (is (= (route S A B T)
         (backtrack-path S T {T (path-info 30 B)
                              B (path-info 20 A)
                              A (path-info 10 S)}))))

(deftest shortest-path-dijkstra-test
  (testing "n = 2"
    (let [g (weighted-graph [A B] [A B 1])]
      (is (= (route A B) (shortest-path-dijkstra g A B)))))

  (testing "n = 3, direct"
    (let [g (weighted-graph [A B C] [A B 3] [B C 5] [A C 7])]
      (is (= (route A C) (shortest-path-dijkstra g A C)))))

  (testing "n = 3, around"
    (let [g (weighted-graph [A B C] [A B 3] [B C 5] [A C 9])]
      (is (= (route A B C) (shortest-path-dijkstra g A C)))))

  (testing "n = 4"
    (let [g (weighted-graph [A B C D] [A B 3] [B C 5] [A C 7] [B D 10] [C D 2])]
      (is (= (route A C D) (shortest-path-dijkstra g A D))))))

(deftest shortest-path-dijkstra-no-path-test
  (let [g (weighted-graph [A B C] [A B 3])]
    (is (thrown-with-msg? Exception #"no path" (shortest-path-dijkstra g A C)))))

(deftest shortest-path-dijkstra-example-test
  (let [path (shortest-path-dijkstra example-graph S T)]
    (is (contains? #{(route S A D T) (route S C D T)} path))
    (is (= 8 (route-length example-graph path)))))

(deftest shortest-path-dijkstra-negative-test
  (let [path (shortest-path-dijkstra example-negative-graph S T)]
    (is (= (route S A T) path))
    (is (= 4 (route-length example-negative-graph path)))))

(deftest shortest-path-modified-dijkstra-test
  (testing "n = 2"
    (let [g (weighted-graph [A B] [A B 1])]
      (is (= (route A B) (shortest-path-modified-dijkstra g A B)))))

  (testing "n = 3, direct"
    (let [g (weighted-graph [A B C] [A B 3] [B C 5] [A C 7])]
      (is (= (route A C) (shortest-path-modified-dijkstra g A C)))))

  (testing "n = 3, around"
    (let [g (weighted-graph [A B C] [A B 3] [B C 5] [A C 9])]
      (is (= (route A B C) (shortest-path-modified-dijkstra g A C)))))

  (testing "n = 4"
    (let [g (weighted-graph [A B C D] [A B 3] [B C 5] [A C 7] [B D 10] [C D 2])]
      (is (= (route A C D) (shortest-path-modified-dijkstra g A D))))))

(deftest shortest-path-modified-dijkstra-no-path-test
  (let [g (weighted-graph [A B C] [A B 3])]
    (is (thrown-with-msg? Exception #"no path" (shortest-path-modified-dijkstra g A C)))))

(deftest shortest-path-modified-dijkstra-example-test
  (let [path (shortest-path-modified-dijkstra example-graph S T)]
    (is (contains? #{(route S A D T) (route S C D T)} path))
    (is (= 8 (route-length example-graph path)))))

(deftest shortest-path-modified-dijkstra-negative-test
  (let [path (shortest-path-modified-dijkstra example-negative-graph S T)]
    (is (= (route S B A T) path))
    (is (= 3 (route-length example-negative-graph path)))))

(deftest shortest-path-modified-dijkstra-exit-test
  (let [g (weighted-graph [S A T] [S T 1] [S A 3] [A T -3])]
    (is (= (route S A T) (shortest-path-modified-dijkstra g S T)))))
