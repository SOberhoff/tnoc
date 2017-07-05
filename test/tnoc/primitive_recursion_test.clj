(ns tnoc.primitive-recursion-test
  (:require [tnoc.primitive-recursion :refer :all]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.properties :as tcprop]
            [clojure.math.numeric-tower :refer [expt]]))

(defspec add-test 100
         (tcprop/for-all [x (tcgen/choose 0 100)
                          y (tcgen/choose 0 100)]
                         (= (+ x y) (add x y))))

(defspec mult-test 100
         (tcprop/for-all [x (tcgen/choose 0 100)
                          y (tcgen/choose 0 100)]
                         (= (* x y) (mult x y))))

(defspec exp-test 100
         (tcprop/for-all [x (tcgen/choose 0 20)
                          y (tcgen/choose 0 5)]
                         (= (expt x y) (exp x y))))

(defspec pred-test 100
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (zero? x) x (dec x)) (pred x))))

(defspec sub-test 100
         (tcprop/for-all [x (tcgen/choose 0 1000)
                          y (tcgen/choose 0 1000)]
                         (= (if (< x y) 0 (- x y)) (sub x y))))

(defspec pos-test 100
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (zero? x) x 1) (pos x))))

(defspec leq-test 100
         (tcprop/for-all [x (tcgen/choose 0 1000)
                          y (tcgen/choose 0 1000)]
                         (= (if (<= x y) 1 0) (leq x y))))

(defspec Z?-test 100
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (zero? x) 1 0) (Z? x))))

(defspec even-test 100
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (if (even? x) 1 0) (even x))))

(defspec div2-test 100
         (tcprop/for-all [x (tcgen/choose 0 1000)]
                         (= (quot x 2) (div2 x))))

(deftest lg-test
  (doseq [x (range 20)]
    (is (= (= (if (zero? x) 0 (int (/ (Math/log x) (Math/log 2)))) (lg x))))))
