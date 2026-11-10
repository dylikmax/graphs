(ns directed.arc-test
  (:require [clojure.test :refer [deftest is]]
            [vertices :refer :all])
  (:use directed.arc))

(deftest arc-test
  (let [a (arc A B)]
    (is (= A (start a)))
    (is (= B (end a)))
    (is (= [A B] (ends a))))
  (is (thrown? AssertionError (arc 1 1))))

(deftest make-arc-test
  (is (= (arc C D) (make-arc [C D]))))
