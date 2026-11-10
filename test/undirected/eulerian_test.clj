(ns undirected.eulerian-test
  (:require [clojure.test :refer :all]
            [vertices :refer :all]
            [undirected.graph :refer [graph complete-graph]]
            [undirected.route :as r :refer [route]])
  (:use undirected.eulerian))

(deftest odd-vertices-test
  (is (empty? (odd-vertices (graph [A]))))
  (is (empty? (odd-vertices (graph [A B C] [A B] [B C] [C A]))))
  (is (= 2 (count (odd-vertices (graph [A B] [A B])))))
  (is (= 4 (count (odd-vertices (graph [A B C D] [A B] [C D]))))))

(deftest eulerian?-test
  (is (eulerian? (graph [A])))
  (is (eulerian? (graph [A B C] [A B] [B C] [C A])))
  (is (not (eulerian? (graph [A B] [A B]))))
  (is (not (eulerian? (graph [A B C D] [A B] [B C] [C A])))))

(deftest semi-eulerian?-test
  (is (semi-eulerian? (graph [A])))
  (is (semi-eulerian? (graph [A B C] [A B] [B C] [C A])))
  (is (semi-eulerian? (graph [A B] [A B])))
  (is (not (semi-eulerian? (complete-graph [A B C D]))))
  (is (not (semi-eulerian? (graph [A B C D] [A B] [B C] [C A])))))

(deftest start-cycle-from-test
  (is (= (route C D A B C)
         (rotate-cycle-to-start-from (route A B C D A) C)))
  (is (= (route A B C A)
         (rotate-cycle-to-start-from (route A B C A) A))))

(deftest merge-routes-at-test
  (is (= (route A B D E B C A)
         (merge-routes-at (route A B C A) (route B D E B) B))))

(deftest merge-routes-test
  (is (= (route A B D E B C A)
         (merge-routes (route A B C A) (route B D E B)))))

(deftest find-longest-route-1-test
  (let [g (graph [A B C D] [A B] [B C] [C D])]
    (is (= (route A B C D) (find-longest-route g A)))
    (is (= (route D C B A) (find-longest-route g D)))))

(deftest find-longest-route-2-test
  ;; 1 - 2
  ;;  \ / \
  ;;   3 - 4
  (let [g (graph [1 2 3 4] [1 2] [1 3] [2 3] [2 4] [3 4])
        r (find-longest-route g 2)]
    (is (= 5 (r/length r)))
    (is (= 3 (r/end r)))))

(deftest find-eulerian-cycle-not-eulerian-test
  (is (thrown? AssertionError (find-eulerian-cycle (graph [A B] [A B])))))

(deftest find-eulerian-cycle-zero-steps-test
  (is (= (route A) (find-eulerian-cycle (graph [A])))))

(deftest find-eulerian-cycle-1-test
  ;; 1 - 2
  ;;  \ /
  ;;   3
  (let [g (graph [1 2 3] [1 2] [2 3] [3 1])
        c (find-eulerian-cycle g)]
    (is (r/cycle? c))
    (is (= g (r/route->graph c)))))

(deftest find-eulerian-cycle-2-test
  ;; 1 - 2 - 3
  ;;  \ / \ /
  ;;   4   5
  (let [g (graph [1 2 3 4 5] [1 2] [1 4] [2 3] [2 4] [2 5] [3 5])
        c (find-eulerian-cycle g)]
    (is (r/cycle? c))
    (is (= g (r/route->graph c)))))

(deftest find-eulerian-chain-not-eulerian-test
  (is (thrown? AssertionError (find-eulerian-chain (graph [A B C D] [A B] [A C] [A D])))))

(deftest find-eulerian-chain-zero-steps-test
  (is (= (route A) (find-eulerian-chain (graph [A])))))

(deftest find-eulerian-chain-1-test
  (is (= (route A B) (find-eulerian-chain (graph [A B] [A B])))))

(deftest find-eulerian-chain-2-test
  ;; 1 - 2
  ;;  \ / \
  ;;   3 - 4
  (let [g (graph [1 2 3 4] [1 2] [1 3] [2 3] [2 4] [3 4])
        c (find-eulerian-chain g)]
    (is (r/chain? c))
    (is (= g (r/route->graph c)))))
