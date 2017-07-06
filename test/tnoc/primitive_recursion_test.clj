(ns tnoc.primitive-recursion-test
  (:require [tnoc.primitive-recursion :refer :all]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.properties :as tcprop]
            [clojure.math.numeric-tower :refer [expt]]))

(defspec add-test
         (tcprop/for-all [x (tcgen/choose 0 100)
                          y (tcgen/choose 0 100)]
                         (= (+ x y) (add x y))))

(defspec mult-test
         (tcprop/for-all [x (tcgen/choose 0 100)
                          y (tcgen/choose 0 100)]
                         (= (* x y) (mult x y))))

(deftest exp-test
  (doseq [x (range 10)
          y (range 5)]
    (is (= (expt x y) (exp x y)))))

(defspec pred-test
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (zero? x) x (dec x)) (pred x))))

(defspec sub-test
         (tcprop/for-all [x (tcgen/choose 0 1000)
                          y (tcgen/choose 0 1000)]
                         (= (if (< x y) 0 (- x y)) (sub x y))))

(defspec pos-test
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (zero? x) x 1) (pos x))))

(defspec leq-test
         (tcprop/for-all [x (tcgen/choose 0 1000)
                          y (tcgen/choose 0 1000)]
                         (= (if (<= x y) 1 0) (leq x y))))

(defspec Z?-test
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (zero? x) 1 0) (Z? x))))

(defspec even-test
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (even? x) 1 0) (even x))))

(defspec div2-test
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (quot x 2) (div2 x))))

(defspec lg-test
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (zero? x) 0 (int (/ (Math/log x) (Math/log 2)))) (lg x))))

(defspec triangle-test
         (tcprop/for-all [x (tcgen/choose 0 200)]
                         (= (reduce + (range (inc x))) (triangle x))))

(defspec pair-test
         (tcprop/for-all [a (tcgen/choose 0 200)
                          b (tcgen/choose 0 200)
                          c (tcgen/choose 0 200)
                          d (tcgen/choose 0 200)]
                         (or (= [a b] [c d]) (not= (pair a b) (pair c d)))))

