(ns tnoc.primrec-test
  (:require [tnoc.primrec :refer :all]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as tcgen]
            [com.gfredericks.test.chuck.properties :refer [for-all]]
            [clojure.math.numeric-tower :refer [expt]]))

(defspec add-test
         (for-all [x (tcgen/choose 0 100)
                   y (tcgen/choose 0 100)]
                  (= (+ x y) (add x y))))

(defspec mult-test
         (for-all [x (tcgen/choose 0 100)
                   y (tcgen/choose 0 100)]
                  (= (* x y) (mult x y))))

(deftest exp-test
  (doseq [x (range 10)
          y (range 5)]
    (is (= (expt x y) (exp x y)))))

(defspec pred-test
         (for-all [x (tcgen/choose 0 1000)]
                  (= (if (zero? x) x (dec x)) (pred x))))

(defspec sub-test
         (for-all [x (tcgen/choose 0 1000)
                   y (tcgen/choose 0 1000)]
                  (= (if (< x y) 0 (- x y)) (sub x y))))

(defspec pos-test
         (for-all [x (tcgen/choose 0 1000)]
                  (= (if (zero? x) x 1) (pos x))))

(defspec leq-test
         (for-all [x (tcgen/choose 0 1000)
                   y (tcgen/choose 0 1000)]
                  (= (if (<= x y) 1 0) (leq x y))))

(defspec Z?-test
         (for-all [x (tcgen/choose 0 1000)]
                  (= (if (zero? x) 1 0) (Z? x))))

(defspec even-test
         (for-all [x (tcgen/choose 0 1000)]
                  (= (if (even? x) 1 0) (even x))))

(defspec div2-test
         (for-all [x (tcgen/choose 0 1000)]
                  (= (quot x 2) (div2 x))))

(defspec lg-test
         (for-all [x (tcgen/choose 0 1000)]
                  (= (if (zero? x) 0 (int (/ (Math/log x) (Math/log 2)))) (lg x))))

(defspec triangle-test
         (for-all [x (tcgen/choose 0 50)]
                  (= (reduce + (range (inc x))) (triangle x))))

(defspec inv-triangle-test
         (for-all [x (tcgen/choose 0 100)]
                  (= (dec (count (take-while #(<= % x) (reductions + (range (inc x))))))
                     (inv-triangle x))))

(defspec pair-test
         (for-all [a (tcgen/choose 0 25)
                   b (tcgen/choose 0 25)
                   c (tcgen/choose 0 25)
                   d (tcgen/choose 0 25)]
                  (or (= [a b] [c d]) (not= (pair a b) (pair c d)))))

(defspec left-test
         (for-all [x (tcgen/choose 0 25)
                   y (tcgen/choose 0 25)]
                  (= x (left (pair x y)))))

(defspec right-test
         (for-all [x (tcgen/choose 0 25)
                   y (tcgen/choose 0 25)]
                  (= y (right (pair x y)))))
