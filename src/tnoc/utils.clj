(ns tnoc.utils
  (:require [clojure.spec.alpha :as spec]
            [clojure.math.numeric-tower :refer [ceil]])
  (:import (java.util Random)))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))

(defn index-of [pred seq]
  "Finds the index of the first element satisfying pred. Returns -1 if no matching element can be found."
  (loop [[first & rest] seq
         index 0]
    (cond (pred first) index
          (nil? rest) -1
          :else (recur rest (inc index)))))

(defn log [b x]
  "Computes the logarithm base b of x."
  (/ (Math/log x) (Math/log b)))

(defn next-int [x]
  "Determines the next higher int following x. Returns x + 1 if x is already an integer."
  (let [c (ceil x)]
    (if (zero? (- c x))
      (int (inc x))
      (int c))))

(defn multiple? [a b]
  "Tests if a is an integer multiple of b."
  (zero? (mod a b)))

(defn mod-exp [x y n]
  "Computes (x ^ y) % n using the repeated squaring method."
  (if (zero? y)
    (if (= 1 n) 0 1)
    (mod (if (even? y)
           (#(*' % %) (mod-exp x (/ y 2) n))
           (*' (mod-exp x (dec y) n) x))
         n)))

(defn euclid [a b]
  "Returns a vector [x y gcd] such that x*a + y*b = gcd."
  (if (zero? b)
    [1 0 a]
    (let [k (quot a b)
          [x y gcd] (euclid b (- a (* k b)))]
      [y (- x (* y k)) gcd])))

(defn gcd [a b]
  "The greatest common divisor of a and b."
  (last (euclid a b)))

(defn lcm [a b]
  "The least common multiple of a and b."
  (/ (*' a b) (gcd a b)))

(defn rand-bigint [n]
  "Returns a random BigInt in the in the range 1 (inclusive) to n (exclusive)."
  (let [rnd (Random.)]
    (->> (repeatedly #(BigInteger. (.bitLength (bigint n)) rnd))
         (filter #(< 0 % n))
         (first))))

(defn miller-rabin [p]
  "Applies the Miller-Rabin primality test once,
  yielding a false positive with probability <= 1/2."
  (if (< 1 p)
    (let [a (rand-bigint p)]
      (if (and (= 1 (gcd a p))
               (= 1 (mod-exp a (dec p) p)))
        (loop [k (dec p)]
          (if (= 1 (mod-exp a k p))
            (if (even? k)
              (recur (/ k 2))
              true)
            (= (dec p) (mod-exp a k p))))))))

(defn prime? [p]
  "Tests if p is prime using the Miller-Rabin test,
  yielding a false positive with probability <= 2^-100."
  (if (< 1 p)
    (every? identity (repeatedly 100 #(miller-rabin p)))))

(def primes (filter prime? (iterate inc 2)))
