(ns tnoc.subset-diophantine
  (:require [clojure.math.numeric-tower :refer [expt ceil]]
            [clojure.spec :as spec]
            [clojure.spec.gen :as sgen]
            [clojure.spec.test :as stest]))

(defn prime? [n]
  (->> (range 2 n)
       (filter #(zero? (rem n %)))
       (empty?)))

(defn multiple? [a b]
  "Tests if `a = x * b` for some integer `x`."
  (zero? (mod a b)))

(load "subset_diophantine_spec")

(defn euclid [a b]
  "Modified Euclidean algorithm which finds the gcd and the coefficients
  such that the linear combination x0*a + x1*b = gcd(a,b)"
  (if (zero? b)
    [a [1 0]]
    (let [[gcd [x0 x1]] (euclid b (rem a b))]
      [gcd [x1 (-' x0 (*' x1 (quot a b)))]])))

(defn gcd [a b]
  "Greatest common divisor of a and b."
  (first (euclid a b)))

(defn lcm [a b]
  "Least common multiple of a and b."
  (/ (*' a b) (gcd a b)))

(defn linear-diophantine [a b y]
  "Finds integers x0 and x1 such that x0*a + x1*b = y.
  Returns nil if no solution exists."
  (let [[gcd [x0 x1]] (euclid a b)
        quotient (quot y gcd)]
    (if (zero? (rem y gcd))
      [(*' x0 quotient) (*' x1 quotient)])))

(defn merge-meshes [[a-base a-shift] [b-base b-shift]]
  "Finds a mesh consisting of the intersection of the two input meshes.
  Returns nil if no such mesh exists."
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
        candidate-thetas (map #(second (reduce merge-meshes [[two-to-m x] [other-qs-product 0] [q %]])) (range 1 q))]
    (apply min candidate-thetas)))

(defn subset-diophantine-constants [weights target]
  (let [n (count weights)
        sum (reduce + weights)
        s (- (* 2 target) sum)
        m (int (ceil (/ (Math/log (+ 1 sum (* 2 target))) (Math/log 2))))
        qs (take n (filter prime? (iterate inc 3)))
        thetas (map (partial find-theta m qs weights) (range n))
        H (reduce +' thetas)
        K (reduce *' (map #(expt % m) qs))
        two-to-m+1 (expt 2 (inc m))
        l-1' (*' (inc (quot K two-to-m+1))
                 (+ 3 (quot (-' (*' s s) (*' H H)) (+' (*' -2 H) -1))))
        l-1'' (*' K s s)
        l-1''' (max l-1' l-1'')
        l-1 (+ l-1''' (- (inc K) (mod l-1''' K)))]
    {:n          n
     :sum        sum
     :s          s
     :m          m
     :qs         qs
     :thetas     thetas
     :H          H
     :K          K
     :two-to-m+1 two-to-m+1
     :l-1'       l-1'
     :l-1''      l-1''
     :l-1        l-1}))

(defn subset-diophantine [weights target]
  "see page 150"
  (let [{s          :s
         H          :H
         K          :K
         two-to-m+1 :two-to-m+1
         l-1        :l-1}
        (subset-diophantine-constants weights target)]
    `(~'fn [~'x ~'y] (~'= (~'+ (~'* (~'+ (~'* ~l-1 ~two-to-m+1) (~'* -1 ~K)) ~'x ~'x)
                            (~'* ~two-to-m+1 ~K ~'y))
                       (~'+ (~'* ~l-1 ~two-to-m+1 ~H ~H) (~'* -1 ~K ~s ~s))))))
