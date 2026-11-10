(ns undirected.route-test
  (:require [clojure.test :refer :all]
            [vertices :refer :all]
            [undirected.graph :refer [graph]]
            [undirected.edge :refer [edge]])
  (:use undirected.route))

(deftest make-route-test
  (let [r (route A B C A B)]
    (is (= [A B C A B] (vertices r)))
    (is (= [(edge A B) (edge B C) (edge C A) (edge A B)] (edges r)))
    (is (= 4 (length r)))
    (is (= A (start r)))
    (is (= B (end r)))))

(deftest make-route-zero-edges-test
  (let [r (route A)]
    (is (= [A] (vertices r)))
    (is (= [] (edges r)))
    (is (= 0 (length r)))
    (is (= A (start r)))
    (is (= A (end r)))))

(deftest make-route-different-vertices-test
  (is (thrown? AssertionError (route A B B C))))

(deftest contains-vertex?-test
  (let [r (route A B)]
    (is (contains-vertex? r A))
    (is (contains-vertex? r B))
    (is (not (contains-vertex? r C)))))

(deftest add-vertex-test
  (is (= (route A B) (add-vertex (route A) B)))
  (is (= (route A B C) (add-vertex (route A B) C))))

(deftest subroute?-test
  (is (subroute? (route A B) (route A B)))
  (is (subroute? (route A B) (route A B C B)))
  (is (subroute? (route A B D) (route A B C B D)))
  (is (not (subroute? (route B A) (route A B))))
  (is (not (subroute? (route A D C) (route A B C))))
  (is (not (subroute? (route C B A) (route A B C))))
  (is (not (subroute? (route A B C A) (route A C B A))))
  (is (not (subroute? (route B C D) (route A B C D E))))
  (is (not (subroute? (route A B C D) (route A B C D E))))
  (is (not (subroute? (route B C D E) (route A B C D E))))
  (is (not (subroute? (route A C E) (route A B C D E)))))

(deftest route->graph-test
  (is (= (graph [A B C] [A B] [B C])
         (route->graph (route A B C))))
  (is (= (graph [A B C] [A B] [B C])
         (route->graph (route A B C B A))))
  (is (= (graph [A B C] [A B] [B C] [C A])
         (route->graph (route A B C A B)))))

(deftest graph-contains-route-test
  (let [g (graph [A B C D] [A B] [B C] [C A])]
    (is (graph-contains-route? g (route A B C)))
    (is (graph-contains-route? g (route A B C A B)))
    (is (not (graph-contains-route? g (route A D))))))

(deftest routes-same-graphs?-test
  (is (routes-same-graph? (route A B C) (route C B A)))
  (is (not (routes-same-graph? (route A B C) (route B A C))))
  (is (routes-same-graph? (route A B C A) (route A C B A)))
  (is (routes-same-graph? (route A B C A) (route B C A B)))
  (is (routes-same-graph? (route A B C A) (route C B A C))))

(deftest chain?-test
  (is (chain? (route A)))
  (is (chain? (route A B C)))
  (is (chain? (route A B C A)))
  (is (chain? (route A B C D B E)))
  (is (not (chain? (route A B A))))
  (is (not (chain? (route A B C A B)))))

(deftest simple-chain?-test
  (is (simple-chain? (route A)))
  (is (simple-chain? (route A B C)))
  (is (simple-chain? (route A B C A)))
  (is (not (simple-chain? (route A B A))))
  (is (not (simple-chain? (route A B C D B))))
  (is (not (simple-chain? (route A B C D B E)))))

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
  (is (not (cycle? (route A B A))))
  (is (not (cycle? (route A B C A B C A))))
  (is (not (cycle? (route A B C)))))

(deftest simple-cycle?-test
  (is (simple-cycle? (route A)))
  (is (simple-cycle? (route A B C A)))
  (is (not (simple-cycle? (route A B A))))
  (is (not (simple-cycle? (route A B C D B))))
  (is (not (simple-cycle? (route A B C D B E A)))))

(deftest example-graph-test
  ;;     2 -- 3
  ;;   /  \ /  \
  ;;  1 -- 5 -- 4
  (let [g (graph [1 2 3 4 5] [1 2] [1 5] [2 5] [2 3] [3 4] [3 5] [4 5])
        r1 (route 1 2 5 1 2)
        r2 (route 1 2 5 3 4 5)
        r3 (route 1 2 5 3 4 5 1)
        r4 (route 1 2 3 4)
        r5 (route 1 2 3 4 5 1)]

    (is (graph-contains-route? g r1))
    (is (not (chain? r1)))

    (is (graph-contains-route? g r2))
    (is (chain? r2))
    (is (not (simple-chain? r2)))

    (is (graph-contains-route? g r3))
    (is (cycle? r3))
    (is (not (simple-cycle? r3)))

    (is (graph-contains-route? g r4))
    (is (simple-chain? r4))

    (is (graph-contains-route? g r5))
    (is (simple-cycle? r5))))

(deftest extract-simple-chain-test
  (is (thrown? AssertionError (extract-simple-chain (route A B A))))
  (is (= (route A B C)
         (extract-simple-chain (route A B C))))
  (is (= (route A D E)
         (extract-simple-chain (route A B C A D E))))
  (is (= (route A B E)
         (extract-simple-chain (route A B C D B E))))
  (is (= (route A B C)
         (extract-simple-chain (route A B C D E C)))))

(deftest extract-simple-cycle-test
  (is (thrown? AssertionError (extract-simple-cycle (route A B C))))
  (is (= (route A B C A)
         (extract-simple-cycle (route A B C A))))
  (is (= (route A B C A)
         (extract-simple-cycle (route A B C A D E A))))
  (is (= (route A B E A)
         (extract-simple-cycle (route A B C D B E A))))
  (is (= (route A B C A)
         (extract-simple-cycle (route A B C D E C A)))))

(deftest find-simple-cycle-test
  (is (thrown? AssertionError (find-simple-cycle (graph [A B C] [A B] [B C]))))
  (let [c (find-simple-cycle (graph [A B C] [A B] [B C] [C A]))]
    (is (simple-cycle? c))
    (is (routes-same-graph? c (route A B C A)))))

(deftest find-simple-cycle-two-possible-cycles-test
  (let [c (find-simple-cycle (graph [A B C D] [A B] [B C] [C D] [D B] [A D]))]
    (is (simple-cycle? c))
    (is (some (fn [r] (routes-same-graph? r c))
              [(route A B C D A) (route A D B A)]))))

(deftest find-simple-cycle-not-the-first-vertex-test
  ;;  7 -- 6 -- 1 -- 2 -- 3
  ;;   \  /          |    |
  ;;    8            5 -- 4
  (let [g (graph [1 2 3 4 5 6 7 8]
                 [1 2] [2 3] [3 4] [4 5] [5 2] [1 6] [6 7] [7 8] [8 6])
        c (find-simple-cycle g)]
    (is (simple-cycle? c))
    (is (some (fn [r] (routes-same-graph? r c))
              [(route 2 3 4 5 2) (route 6 7 8 6)]))))
