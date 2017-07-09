(ns tnoc.three-sat-diophantine
  (:require [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.spec :as spec]
            [clojure.spec.gen :as sgen]))

(defn preprocess [formula]
  (->> formula
       (distinct)
       (filter (fn [clause]
                 (every? not (for [x clause, y clause]
                               (or (and (seq? x) (= (second x) y))
                                   (and (seq? y) (= (second y) x)))))))))

(defn get-variables [formula]
  (->> formula
       (flatten)
       (distinct)
       (filter (comp not #{'!}))))

(defn compute-tau [formula]
  (reduce + (map #(*' -1 (expt 8 %)) (range 1 (inc (count formula))))))

(defn compute-fi's [formula]
  (->> formula
       (get-variables)
       (map (fn [var]
              (let [fi+ (reduce +' (map-indexed #(if (some #{var} %2) (expt 8 (inc %1)) 0) formula))
                    fi- (reduce +' (map-indexed #(if (some #{(list '! var)} %2) (expt 8 (inc %1)) 0) formula))]
                {:var var :fi+ fi+ :fi- fi-})))))

(defn compute-n [formula] (+ (* 2 (count formula) (count (get-variables formula)))))

(defn compute-cj's [formula]
  (let [c0 1
        c1-to-2m (->> (range 1 (inc (count formula)))
                      (map #(if (even? %)
                              (*' -1/2 (expt 8 (/ (dec %) 2)))
                              (*' -1 (expt 8 (/ % 2))))))]))