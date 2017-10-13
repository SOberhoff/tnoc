(ns tnoc.ip
  (:require [clojure.spec.alpha :as spec]))

(load "ip_spec")

(defn get-degree [term]
  (->> term
       (filter coll?)
       (map second)
       (reduce +)))

(defn simplify-term [[coefficient & variables]]
  (->> variables
       (reduce (fn [base->powers [base power]]
                 (assoc base->powers base (+ (base->powers base 0) power)))
               {})
       (seq)
       (sort-by first)
       (concat [coefficient])))

(defn simplify-sum [terms]
  (->> (map simplify-term terms)
       (reduce (fn [factors->coefficient [coefficient & variables]]
                 (assoc factors->coefficient variables (+ (factors->coefficient variables 0) coefficient)))
               {})
       (map #(concat [(second %)] (first %)))
       (sort-by get-degree)
       (reverse)))

(defn multiply-sums [sum-0 sum-1]
  (for [[coefficient-0 & variables-0] sum-0
        [coefficient-1 & variables-1] sum-1]
    (concat [(* coefficient-0 coefficient-1)] variables-0 variables-1)))

(defn simplify-polynomial [polynomial]
  (cond
    (spec/valid? ::term polynomial) (simplify-term polynomial)
    (spec/valid? ::sum polynomial) (->> (rest polynomial)
                                        (map simplify-polynomial)
                                        (reduce #(if (spec/valid? ::term %2)
                                                   (concat %1 [%2])
                                                   (concat %1 (rest %2)))
                                                ['+]))
    (spec/valid? ::product polynomial) (->> (rest polynomial)
                                            (map simplify-polynomial)
                                            (map #(if (spec/valid? ::term %) [%] (rest %)))
                                            (reduce multiply-sums)
                                            (simplify-sum)
                                            (concat ['+]))))


(def t `(~'* (~'+ ~[2 [:x 2]] ~[5]) (~'+ ~[3 [:x 5]] ~[2 [:x 1]])))

(simplify-polynomial t)
