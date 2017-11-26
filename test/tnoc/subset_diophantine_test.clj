(ns tnoc.subset-diophantine-test
  (:require [tnoc.utils :refer [multiple?]]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.properties :as tcprop]
            [tnoc.subset-diophantine :refer :all]
            [clojure.math.numeric-tower :refer [expt sqrt]]))

(def weights-gen (tcgen/vector tcgen/s-pos-int 1 10))

(defn subset-sum [weights target]
  (->> weights
       (reduce (fn [selections element] (into selections (map #(conj % element) selections))) #{[]})
       (filter #(= target (reduce + %)))))

(defn subset-sum? [weights target]
  (boolean (not-empty (subset-sum weights target))))


(defn expr-5-2|5-4? [S m [weight & weights]]
  (if weight
    (or (expr-5-2|5-4? (+ S weight) m weights) (expr-5-2|5-4? (- S weight) m weights))
    (multiple? S (expt 2 m))))

(defspec expr-5-2-test
         (tcprop/for-all [weights weights-gen
                          target tcgen/pos-int]
                         (let [{S :S m :m} (subset-diophantine-constants weights target)]
                           (= (subset-sum? weights target)
                              (expr-5-2|5-4? S m (weights-with-odd-sum weights target))))))

(defspec expr-5-3-test
         (tcprop/for-all [[weights index] (tcgen/let [weights weights-gen
                                                      index (tcgen/choose 0 (dec (count weights)))]
                                                     [weights index])]
                         (let [{m :m qs :qs thetas :thetas} (subset-diophantine-constants weights 0)
                               q (nth qs index)
                               theta (nth thetas index)]
                           (and (= (nth weights index) (mod theta (expt 2 m)))
                                (multiple? theta (reduce *' (map #(expt % m) (remove #{q} qs))))
                                (not (multiple? theta q))))))

(defspec expr-5-4-test
         (tcprop/for-all [weights weights-gen
                          target tcgen/s-pos-int]
                         (let [{S :S m :m thetas :thetas} (subset-diophantine-constants weights target)]
                           (= (subset-sum? weights target)
                              (expr-5-2|5-4? S m thetas)))))

(defspec two-H-less-than-K-test
         (tcprop/for-all [weights weights-gen]
                         (let [{H :H K :K} (subset-diophantine-constants weights 0)]
                           (< (*' 2 H) K))))

(defspec qs-dont-divide-H-test
         (tcprop/for-all [weights weights-gen]
                         (let [{H  :H
                                qs :qs} (subset-diophantine-constants weights 0)]
                           (every? #(not (multiple? H %)) qs))))

(defn find-lemma-5-1 [H K S m]
  (->> (*' H H)
       (iterate #(- % K))
       (take-while #(<= 0 %))
       (map sqrt)
       (filter #(and (integer? %)
                     (<= % H)
                     (multiple? (- (*' S S) (*' % %)) (expt 2 (inc m)))))))

(def lemma-5-1-gen
  (tcgen/let [weights (tcgen/such-that #(odd? (reduce + %))
                                       (tcgen/vector (tcgen/resize 6 tcgen/s-pos-int) 1 2))
              target (tcgen/resize 4 tcgen/pos-int)]
             [weights target]))

(defspec lemma-5-1-test
         (tcprop/for-all [[weights target] lemma-5-1-gen]
                         (let [{H :H
                                K :K
                                S :S
                                m :m} (subset-diophantine-constants weights target)]
                           (= (empty? (subset-sum weights target))
                              (empty? (find-lemma-5-1 H K S m))))))

(defn get-sigmas [[next-solution & rem-solution :as solution] [next-weight & rem-weights]]
  (if (some? next-weight)
    (if (= next-solution next-weight)
      (concat [1] (get-sigmas rem-solution rem-weights))
      (concat [-1] (get-sigmas solution rem-weights)))))

(defn find-X [weights target]
  (let [weights (weights-with-odd-sum weights target)
        {thetas :thetas} (subset-diophantine-constants weights target)
        sigmas (get-sigmas (first (subset-sum weights target)) weights)]
    (reduce +' (map * thetas sigmas))))

(def solvable-subset-sum-gen
  (tcgen/such-that
    #(apply subset-sum? %)
    (tcgen/tuple weights-gen tcgen/s-pos-int)
    10000))

(defspec diophantine-subset-test 20
         (tcprop/for-all [[weights target] solvable-subset-sum-gen]
                         (let [f (subset-diophantine weights target)
                               [a b c] (filter number? (flatten f))
                               X (find-X weights target)
                               Y (/ (- c (*' a X X)) b)]
                           ((eval f) X Y))))
