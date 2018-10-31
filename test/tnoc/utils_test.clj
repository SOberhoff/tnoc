(ns tnoc.utils-test
  (:require [tnoc.utils :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck.properties :refer [for-all]]
            [clojure.test.check.generators :as tcgen]
            [clojure.math.numeric-tower :refer [expt]]))

(defspec mod-exp-test
         (for-all [x (tcgen/choose 0 20)
                   y (tcgen/choose 0 5)
                   n (tcgen/choose 1 1000)]
                  (= (mod (expt x y) n) (mod-exp x y n))))

(defspec euclid-test
         (for-all [a tcgen/pos-int
                   b tcgen/pos-int]
                  (let [[x y gcd] (euclid a b)]
                    (= gcd (+ (* x a) (* y b))))))


(defn- prime?-brute-force [n]
  (if (< 1 n)
    (->> (range 2 n)
         (not-any? #(zero? (mod n %))))))

(defspec prime?-test
         (for-all [n (tcgen/choose 1 1000)]
                  (= (prime?-brute-force n) (prime? n))))
