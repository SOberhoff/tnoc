(ns tnoc.edit-distance
  (:require [clojure.spec.alpha :as spec]))

(load "edit_distance_spec")

(def edit-distance
  "Computes the edit distance between two strings s and t as described in Section 3.3.
  Shifts are denoted using the underscore character."
  (memoize
    (fn [[s1 & s' :as s] [t1 & t' :as t]]
      (cond
        (empty? s) [(count t) (apply str (repeat (count t) "_")) (apply str t)]
        (empty? t) [(count s) (apply str s) (apply str (repeat (count s) "_"))]
        :else (let [[cost-1 s-rem-1 t-rem-1] (edit-distance s' t)
                    [cost-2 s-rem-2 t-rem-2] (edit-distance s t')
                    [cost-3 s-rem-3 t-rem-3] (edit-distance s' t')]
                (min-key first
                         [(inc cost-1) (str s1 s-rem-1) (str "_" t-rem-1)]
                         [(inc cost-2) (str "_" s-rem-2) (str t1 t-rem-2)]
                         [(if (= s1 t1) cost-3 (inc cost-3)) (str s1 s-rem-3) (str t1 t-rem-3)]))))))