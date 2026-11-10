(ns intro
  (:require [utils :refer [???]]))

(defn square [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn factorial [n]
  (loop [product 1, k 1]
    (if (> k n)
      product
      (recur (* product k) (inc k)))))

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))
