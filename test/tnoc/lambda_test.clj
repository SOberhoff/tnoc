(ns tnoc.lambda-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as tcgen]
            [com.gfredericks.test.chuck.properties :refer [for-all]]
            [tnoc.lambda :refer :all]
            [clojure.math.numeric-tower :refer [expt]]))

(deftest not-test
  (is (= T (normalize-simplified `(NOT F))))
  (is (= F (normalize-simplified `(NOT T)))))

(deftest and-test
  (is (= F (normalize `(AND F F))))
  (is (= F (normalize `(AND F T))))
  (is (= F (normalize `(AND T F))))
  (is (= T (normalize `(AND T T)))))

(deftest or-test
  (is (= F (normalize `(OR F F))))
  (is (= T (normalize `(OR F T))))
  (is (= T (normalize `(OR T F))))
  (is (= T (normalize `(OR T T)))))

(defspec succ-test
         (for-all [x (tcgen/choose 0 1000)]
                  (= (inc x) (unchurch (normalize `(SUCC ~x))))))

(defspec add-test 20
         (for-all [x (tcgen/choose 0 20) y (tcgen/choose 0 20)]
                  (= (+ x y) (unchurch (normalize `(ADD ~x ~y))))))

(defspec add'-test 20
         (for-all [x (tcgen/choose 0 20) y (tcgen/choose 0 20)]
                  (= (+ x y) (unchurch (normalize `(ADD' ~x ~y))))))

(defspec mult-test 20
         (for-all [x (tcgen/choose 0 20) y (tcgen/choose 0 20)]
                  (= (* x y) (unchurch (normalize `(MULT ~x ~y))))))

(defspec mult'-test 20
         (for-all [x (tcgen/choose 0 20) y (tcgen/choose 0 20)]
                  (= (* x y) (unchurch (normalize `(MULT' ~x ~y))))))

(defspec expt-test 20
         (for-all [x (tcgen/choose 0 4) y (tcgen/choose 1 4)]
                  (= (expt x y) (unchurch (normalize `(EXP ~x ~y))))))

(defspec first-test
         (for-all [x (tcgen/choose 0 100) y (tcgen/choose 0 100)]
                  (= x (unchurch (normalize `(FIRST (PAIR ~x ~y)))))))

(defspec second-test
         (for-all [x (tcgen/choose 0 100) y (tcgen/choose 0 100)]
                  (= y (unchurch (normalize `(SECOND (PAIR ~x ~y)))))))

(defspec pred-test
         (for-all [x (tcgen/choose 0 100)]
                  (= (if (zero? x) 0 (dec x)) (unchurch (normalize `(PRED ~x))))))

(deftest zero-test
  (is (= T (normalize `(ZERO? 0)))))

(defspec not-zero-test
         (for-all [x (tcgen/choose 1 100)]
                  (= F (normalize `(ZERO? ~x)))))

(deftest fac-test
  (is (= 1 (unchurch (normalize `(FAC 0)))))
  (is (= 1 (unchurch (normalize `(FAC 1)))))
  (is (= 2 (unchurch (normalize `(FAC 2)))))
  (is (= 6 (unchurch (normalize `(FAC 3)))))
  (is (= 24 (unchurch (normalize `(FAC 4))))))

(deftest fib-test
  (is (= 1 (unchurch (normalize `(FIB 0)))))
  (is (= 1 (unchurch (normalize `(FIB 1)))))
  (is (= 2 (unchurch (normalize `(FIB 2)))))
  (is (= 3 (unchurch (normalize `(FIB 3)))))
  (is (= 5 (unchurch (normalize `(FIB 4)))))
  (is (= 8 (unchurch (normalize `(FIB 5)))))
  (is (= 13 (unchurch (normalize `(FIB 6)))))
  (is (= 21 (unchurch (normalize `(FIB 7))))))
