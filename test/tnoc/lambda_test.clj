(ns tnoc.lambda-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.properties :as tcprop]
            [tnoc.lambda :refer :all]
            [clojure.math.numeric-tower :refer [expt]]))

(deftest not-test
  (is (= T (normal-form-simplified `(NOT F))))
  (is (= F (normal-form-simplified `(NOT T)))))

(deftest and-test
  (is (= F (normal-form `(AND F F))))
  (is (= F (normal-form `(AND F T))))
  (is (= F (normal-form `(AND T F))))
  (is (= T (normal-form `(AND T T)))))

(deftest or-test
  (is (= F (normal-form `(OR F F))))
  (is (= T (normal-form `(OR F T))))
  (is (= T (normal-form `(OR T F))))
  (is (= T (normal-form `(OR T T)))))

(defspec succ-test
         (tcprop/for-all [(tcgen/choose 0 1000)]
                         #(= (inc %) (unchurch (normal-form `(SUCC ~%))))))

(defspec add-test 20
         (tcprop/for-all [x (tcgen/choose 0 20) y (tcgen/choose 0 20)]
                         (= (+ x y) (unchurch (normal-form `(ADD ~x ~y))))))

(defspec add'-test 20
         (tcprop/for-all [x (tcgen/choose 0 20) y (tcgen/choose 0 20)]
                         (= (+ x y) (unchurch (normal-form `(ADD' ~x ~y))))))

(defspec mult-test 20
         (tcprop/for-all [x (tcgen/choose 0 20) y (tcgen/choose 0 20)]
                         (= (* x y) (unchurch (normal-form `(MULT ~x ~y))))))

(defspec mult'-test 20
         (tcprop/for-all [x (tcgen/choose 0 20) y (tcgen/choose 0 20)]
                         (= (* x y) (unchurch (normal-form `(MULT' ~x ~y))))))

(defspec expt-test 20
         (tcprop/for-all [x (tcgen/choose 0 4) y (tcgen/choose 1 4)]
                         (= (expt x y) (unchurch (normal-form `(EXP ~x ~y))))))

(defspec first-test
         (tcprop/for-all [x (tcgen/choose 0 100) y (tcgen/choose 0 100)]
                         (= x (unchurch (normal-form `(FIRST (PAIR ~x ~y)))))))

(defspec second-test
         (tcprop/for-all [x (tcgen/choose 0 100) y (tcgen/choose 0 100)]
                         (= y (unchurch (normal-form `(SECOND (PAIR ~x ~y)))))))

(defspec pred-test
         (tcprop/for-all [(tcgen/choose 0 100)]
                         #(= (dec %) (unchurch (normal-form `(PRED ~%))))))

(deftest zero-test
  (is (= T (normal-form `(ZERO? 0)))))

(defspec not-zero-test
         (tcprop/for-all [(tcgen/choose 1 100)]
                         #(= F (normal-form `(ZERO? ~%)))))

(deftest fac-test
  (is (= 1 (unchurch (normal-form `(FAC 0)))))
  (is (= 1 (unchurch (normal-form `(FAC 1)))))
  (is (= 2 (unchurch (normal-form `(FAC 2)))))
  (is (= 6 (unchurch (normal-form `(FAC 3)))))
  (is (= 24 (unchurch (normal-form `(FAC 4))))))

(deftest fib-test
  (is (= 1 (unchurch (normal-form `(FIB 0)))))
  (is (= 1 (unchurch (normal-form `(FIB 1)))))
  (is (= 2 (unchurch (normal-form `(FIB 2)))))
  (is (= 3 (unchurch (normal-form `(FIB 3)))))
  (is (= 5 (unchurch (normal-form `(FIB 4)))))
  (is (= 8 (unchurch (normal-form `(FIB 5)))))
  (is (= 13 (unchurch (normal-form `(FIB 6)))))
  (is (= 21 (unchurch (normal-form `(FIB 7))))))
