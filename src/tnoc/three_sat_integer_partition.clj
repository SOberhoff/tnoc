(ns tnoc.three-sat-integer-partition
  (:require [tnoc.subset-diophantine :refer [prime? merge-meshes]]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :refer [expt]]
            [rolling-stones.core :refer [!]]
            [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as sgen])
  (:import (rolling_stones.core Not)))

(load "three_sat_integer_partition_spec")

(defn cleanup [formula]
  (->> formula
       (distinct)
       (filter (fn [clause]
                 (every? not (for [x clause, y clause]
                               (or (and (seq? x) (= (second x) y))
                                   (and (seq? y) (= (second y) x)))))))))

(defn get-variables [formula]
  (->> formula
       (flatten)
       (map #(if (instance? Not %) (:literal %) %))
       (distinct)
       (filter (comp not #{'!}))))

(defn compute-fi's [formula]
  (->> formula
       (get-variables)
       (map (fn [var]
              (let [fi+ (reduce +' (map-indexed #(if (some #{var} %2) (expt 8 (inc %1)) 0) formula))
                    fi- (reduce +' (map-indexed #(if (some #{(! var)} %2) (expt 8 (inc %1)) 0) formula))]
                {:var var :fi+ fi+ :fi- fi-})))))

(defn compute-cj's [formula]
  (let [c1-to-2m (->> (range 1 (inc (* 2 (count formula))))
                      (map #(if (odd? %)
                              (*' -1/2 (expt 8 (/ (inc %) 2)))
                              (*' -1 (expt 8 (/ % 2))))))
        c2m+1-to-2m+l (->> (compute-fi's formula)
                           (map #(*' 1/2 (-' (:fi+ %) (:fi- %)))))]
    (concat [1] c1-to-2m c2m+1-to-2m+l)))

(defn compute-tau [formula]
  (let [tau-phi (*' -1 (reduce +' (map #(expt 8 %) (range 1 (inc (count formula))))))
        cj-sum (reduce +' (compute-cj's formula))
        fi--sum (reduce +' (map :fi- (compute-fi's formula)))]
    (+' tau-phi cj-sum fi--sum)))

(defn three-sat-integer-partition [formula]
  (let [formula (cleanup formula)]
    [(compute-cj's formula) (compute-tau formula)]))
