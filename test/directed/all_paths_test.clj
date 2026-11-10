(ns directed.all-paths-test
  (:require [clojure.test :refer :all]
            [directed.route :refer [route]]
            [directed.weighted-graph :refer [route-length weighted-graph]]
            [vertices :refer :all])
  (:use [directed.all-paths]))

(deftest path-info-test
  (let [path-map {[A B] (path-info 15 B)}]
    (is (= 15 (value path-map A B)))
    (is (= B (step path-map A B)))))

(deftest initial-path-map-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 2] [C A 5])
        path-map (initial-path-map g)]
    (is (= {[A B] (path-info 1 B)
            [B C] (path-info 2 C)
            [C A] (path-info 5 A)} path-map))))

(deftest update-path-length-test
  ;;   A -- 20 --> B
  ;;   |           ^
  ;;   5           |
  ;;   |           9
  ;;   v           |
  ;;   C -- 5 ---> D
  (let [g (weighted-graph [A B C D] [A B 20] [A C 5] [C D 5] [D B 9])
        path-map-0 (initial-path-map g)
        path-map-C {[A B] (path-info 20 B)
                    [A C] (path-info 5 C)
                    [A D] (path-info 10 C)
                    [C D] (path-info 5 D)
                    [D B] (path-info 9 B)}]
    (is (= path-map-C (into path-map-0 (update-path-length g path-map-0 C))))
    (is (= {[A B] (path-info 19 C)
            [C B] (path-info 14 D)}
           (into {} (update-path-length g path-map-C D))))))

(deftest collect-path-test
  (let [path-map {[A B] (path-info 10 B)
                  [A C] (path-info 15 B)
                  [B C] (path-info 5 C)}]
    (is (= (route A B) (collect-path path-map A B)))
    (is (= (route A B C) (collect-path path-map A C)))
    (is (= nil (collect-path path-map B A)))))

(deftest collect-path-same-vertex-test
  (let [path-map {[A A] (path-info 10 B)
                  [A B] (path-info 5 B)
                  [B A] (path-info 5 A)}]
    (is (= (route A B A) (collect-path path-map A A)))))

(deftest collect-all-paths-test
  (let [g (weighted-graph [A B C] [A B 1] [A C 4] [B C 2])
        path-map {[A B] (path-info 1 B)
                  [A C] (path-info 3 B)
                  [B C] (path-info 2 C)}
        paths (collect-all-paths g path-map)]
    (is (= {[A A] nil, [A B] (route A B), [A C] (route A B C)
            [B A] nil, [B B] nil, [B C] (route B C)
            [C A] nil, [C B] nil, [C C] nil} paths))))

(deftest all-shortest-paths-floyd-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 2] [C A 3])
        paths {[A A] (route A B C A), [A B] (route A B), [A C] (route A B C),
               [B A] (route B C A), [B B] (route B C A B), [B C] (route B C),
               [C A] (route C A), [C B] (route C A B), [C C] (route C A B C)}]
    (is (= paths (all-shortest-paths-floyd g)))))

(deftest all-shortest-paths-floyd-example-test
  (let [g (weighted-graph [1 2 3 4 5]
                          [1 2 7] [1 3 -1] [1 4 9] [1 5 -4]
                          [2 1 2] [2 3 -4] [2 4 7] [2 5 1]
                          [3 1 1] [3 2 4] [3 4 8] [3 5 1]
                          [4 1 4] [4 2 1] [4 3 -5] [4 5 -3]
                          [5 1 9] [5 2 7])
        paths (all-shortest-paths-floyd g)]
    (is (= (route 2 3 1 5) (paths [2 5])))
    (is (= -7 (route-length g (paths [2 5]))))
    (is (= (route 4 3 1 5) (paths [4 5])))
    (is (= -8 (route-length g (paths [4 5]))))
    (is (= (route 5 2 3 4) (paths [5 4])))
    (is (= 11 (route-length g (paths [5 4]))))))

(deftest all-shortest-paths-floyd-negative-circuit-test
  (let [g (weighted-graph [A B C] [A B -1] [B C -2] [C A -3])
        paths {[A A] (route A B C A), [A B] (route A B), [A C] (route A B C),
               [B A] (route B C A), [B B] (route B C A B), [B C] (route B C),
               [C A] (route C A), [C B] (route C A B), [C C] (route C A B C)}]
    (is (= paths (all-shortest-paths-floyd g)))))

(deftest route-width-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 3] [C A 5])]
    (is (= 1 (route-width g (route A B C))))
    (is (= 3 (route-width g (route B C A))))))

(deftest update-path-width-test
  (let [g (weighted-graph [A B C] [A B 1] [A C 4] [B C 2] [C A 5])
        path-map-0 (initial-path-map g)]
    (is (= {[C B] (path-info 1 A)
            [C C] (path-info 4 A)}
           (into {} (update-path-width g path-map-0 A))))
    (is (= {[A A] (path-info 4 C)
            [B A] (path-info 2 C)}
           (into {} (update-path-width g path-map-0 C))))))

(deftest all-widest-paths-floyd-test
  (let [g (weighted-graph [A B C] [A B 1] [B C 2] [A C 3])
        paths {[A A] nil, [A B] (route A B), [A C] (route A C),
               [B A] nil, [B B] nil, [B C] (route B C),
               [C A] nil, [C B] nil, [C C] nil}]
    (is (= paths (all-widest-paths-floyd g)))))

(deftest all-widest-paths-floyd-example-test
  (let [g (weighted-graph [1 2 3 4 5]
                          [1 2 9] [1 3 6] [1 4 1] [1 5 1]
                          [2 1 2] [2 3 7] [2 5 5]
                          [3 1 6] [3 2 3] [3 4 3] [3 5 5]
                          [4 1 8] [4 2 8] [4 5 1]
                          [5 1 7] [5 2 8] [5 3 7] [5 4 2])
        paths (all-widest-paths-floyd g)]
    (is (= (route 1 2 3) (paths [1 3])))
    (is (= 7 (route-width g (paths [1 3]))))
    (is (= (route 2 3 1) (paths [2 1])))
    (is (= 6 (route-width g (paths [2 1]))))))
