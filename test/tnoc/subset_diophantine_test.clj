(ns tnoc.subset-diophantine-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.properties :as tcprop]
            [tnoc.subset-diophantine :refer :all]
            [clojure.math.numeric-tower :refer [expt]]))

(defn subset-sum? [weights target]
  (->> weights
       (reduce (fn [subsets element] (into subsets (map #(conj % element) subsets))) #{#{}})
       (some #(= target (reduce + %)))
       (boolean)))

(defspec find-theta-test
         (tcprop/for-all [[weights index] (tcgen/bind (tcgen/vector tcgen/s-pos-int 1 10)
                                                      #(tcgen/tuple (tcgen/return %) (tcgen/choose 0 (dec (count %)))))]
                         (let [{m :m qs :qs thetas :thetas} (subset-diophantine-constants weights 0)
                               q (nth qs index)
                               theta (nth thetas index)]
                           (and (= (nth weights index) (mod theta (expt 2 m)))
                                (multiple? theta (reduce *' (map #(expt % m) (remove #{q} qs))))
                                (not (multiple? theta q))))))

(defspec two-H-less-than-K-test
         (tcprop/for-all [weights (tcgen/vector tcgen/s-pos-int 1 10)]
                         (let [{H :H K :K} (subset-diophantine-constants weights 0)]
                           (< (*' 2 H) K))))

(defspec qs-dont-divide-H-test
         (tcprop/for-all [weights (tcgen/vector tcgen/s-pos-int 1 10)]
                         (let [{H  :H
                                qs :qs} (subset-diophantine-constants weights 0)]
                           (every? #(not (multiple? H %)) qs))))

(defspec lemma-5-1-test
         (tcprop/for-all [weights (tcgen/vector tcgen/s-pos-int 1 3)
                          target tcgen/s-pos-int]
                         (= (subset-sum? weights target)
                            (let [{H          :H
                                   K          :K
                                   s          :s
                                   two-to-m+1 :two-to-m+1}
                                  (subset-diophantine-constants weights target)]
                              (reduce (fn [_ next]
                                        (if (and (multiple? (-' ('* H H) ('* next next)) K)
                                                 (multiple? (-' ('* s s) ('* next next)) two-to-m+1))
                                          (reduced true) false))
                                      false (range (inc H)))))))

(subset-diophantine-constants [1 1] 1)

(let [vals (subset-diophantine-constants [1 1] 3)]
  (def H (:H vals))
  (def K (:K vals))
  (def s (:s vals))
  (def two-to-m+1 (:two-to-m+1 vals)))

(map (fn [next]
       (if (and (multiple? (-' ('* H H) ('* next next)) K)
                (multiple? (-' ('* s s) ('* next next)) two-to-m+1))
         [next true]
         [next false]))
     (range (inc H)))

(lemma-5-1-test)

(defn has-nat-root? [f] (->> (for [x (range 1000) y (range 1000)] [x y])
                             (filter (partial apply (eval f)))
                             (first)))

(defspec diophantine-subset-test 5
         (tcprop/for-all [weights (tcgen/vector tcgen/s-pos-int 1 3)
                          target tcgen/s-pos-int]
                         (= (subset-sum? weights target)
                            (has-nat-root? (subset-diophantine weights target)))))


(has-nat-root? (subset-diophantine [1] 0))