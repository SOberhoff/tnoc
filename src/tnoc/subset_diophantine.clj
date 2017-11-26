(ns tnoc.subset-diophantine
  (:require [tnoc.utils :refer :all]
            [clojure.math.numeric-tower :refer [expt ceil]]
            [clojure.spec.alpha :as spec]))

(load "subset_diophantine_spec")

(defn linear-diophantine [a b y]
  "Finds integers x0 and x1 such that x0*a + x1*b = y.
  Returns nil if no solution exists."
  (let [[x0 x1 gcd] (euclid a b)
        quotient (quot y gcd)]
    (if (multiple? y gcd)
      [(*' x0 quotient) (*' x1 quotient)])))

(defn intersect-1d-lattices [[a-base a-shift] [b-base b-shift]]
  "Finds a 1d-lattice consisting of the intersection of the two input lattices.
  Returns nil if no such lattice exists."
  (if-let [[x0 _] (linear-diophantine a-base (*' -1 b-base) (-' b-shift a-shift))]
    (let [lcm (lcm a-base b-base)
          intersection (+' a-shift (*' a-base x0))
          n (if (neg? intersection) (dec (quot intersection lcm)) (quot intersection lcm))
          min-positive-intersection (-' intersection (*' lcm n))]
      [lcm min-positive-intersection])))

(defn find-theta [m qs weights index]
  (let [two-to-m (expt 2 m)
        x (nth weights index)
        q (nth qs index)
        other-qs-product (reduce *' (map #(expt % m) (remove #{q} qs)))
        candidate-thetas (map #(second (reduce intersect-1d-lattices [[two-to-m x] [other-qs-product 0] [q %]])) (range 1 q))]
    (apply min candidate-thetas)))

(defn weights-with-odd-sum [weights target]
  "Returns a possibly extended list of weights assuring that the sum of the weights is odd. This is
  done by adding the next largest odd number following the target if necessary. Note that this leaves
  the solvability of the problem unchanged."
  (if (even? (reduce +' weights))
    (->> (inc target)
         (iterate inc)
         (filter odd?)
         (first)
         (conj weights))
    weights))

(defn subset-diophantine-constants [weights target]
  "Returns a map containing all the different constants produced during the reduction."
  (let [weights (weights-with-odd-sum weights target)
        n (count weights)
        sum (reduce + weights)
        S (- (* 2 target) sum)
        m (next-int (log 2 (+ sum (Math/abs S))))
        l (next-int (/ (log 3 (*' n (expt 2 (inc m)))) (dec m)))
        qs (take n (map #(expt % l) (rest primes)))
        thetas (map (partial find-theta m qs weights) (range n))
        H (reduce +' thetas)
        K (reduce *' (map #(expt % m) qs))
        lambda-1 (*' (inc K) (inc K))]
    {:n        n
     :sum      sum
     :S        S
     :m        m
     :l        l
     :qs       qs
     :thetas   thetas
     :H        H
     :K        K
     :lambda-1 lambda-1}))

(defn subset-diophantine [weights target]
  "Reduces an instance of Subset Sum to a quadratic Diophantine equation. See page 150 and following."
  (let [{S :S m :m H :H K :K lambda-1 :lambda-1} (subset-diophantine-constants weights target)
        two-to-m+1 (expt 2 (inc m))
        a (-' (*' lambda-1 two-to-m+1) K)
        b (*' two-to-m+1 K)
        c (-' (*' lambda-1 two-to-m+1 H H) (*' K S S))]
    `(fn [~'x ~'y] (= (+' (*' ~a ~'x ~'x) (*' ~b ~'y)) ~c))))
