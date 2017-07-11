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

;(defspec lemma-5-1-test 10
;         (tcprop/for-all [weights (tcgen/vector (tcgen/resize 4 tcgen/s-pos-int) 1 2)
;                          target (tcgen/resize 10 tcgen/s-pos-int)]
;                         (= (subset-sum? weights target)
;                            (let [{H          :H
;                                   K          :K
;                                   s          :s
;                                   two-to-m+1 :two-to-m+1}
;                                  (subset-diophantine-constants weights target)]
;                              (reduce (fn [_ x]
;                                        (if (and (multiple? (-' (*' H H) (*' x x)) K)
;                                                 (multiple? (-' (*' s s) (*' x x)) two-to-m+1))
;                                          (reduced true) false))
;                                      false (range (inc H)))))))
;
;(defn has-nat-root? [f] (->> (for [x (range 1000) y (range 1000)] [x y])
;                             (filter (partial apply (eval f)))
;                             (first)))
;
;(defspec diophantine-subset-test 5
;         (tcprop/for-all [weights (tcgen/vector tcgen/s-pos-int 1 3)
;                          target tcgen/s-pos-int]
;                         (= (subset-sum? weights target)
;                            (has-nat-root? (subset-diophantine weights target)))))
