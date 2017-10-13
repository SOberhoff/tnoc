(ns tnoc.ip
  (:require [clojure.spec.alpha :as spec]
            [clojure.walk :as walk]
            [clojure.math.numeric-tower :refer [expt]]))

(load "ip_spec")

(defn get-degree [term]
  (->> term
       (filter coll?)
       (map second)
       (reduce +)))

(defn simplify-term [term]
  (loop [coefficient 1
         base->powers {}
         [next-factor & remaining-factors] term]
    (if next-factor
      (if (integer? next-factor)
        (recur (* coefficient next-factor) base->powers remaining-factors)
        (let [[base power] next-factor]
          (recur coefficient (assoc base->powers base (+ (base->powers base 0) power)) remaining-factors)))
      (concat [coefficient] (sort-by first (seq base->powers))))))

(defn simplify-sum [terms]
  (->> (map simplify-term terms)
       (reduce (fn [factors->coefficient [coefficient & variables]]
                 (assoc factors->coefficient variables (+ (factors->coefficient variables 0) coefficient)))
               {})
       (map #(concat [(second %)] (first %)))
       (filter (comp not zero? first))
       (sort-by get-degree)
       (reverse)))

(defn add-sums [sum-0 sum-1]
  (if (spec/valid? ::term sum-1)
    (concat sum-0 [sum-1])
    (concat sum-0 (rest sum-1))))

(defn multiply-sums [sum-0 sum-1]
  (for [[coefficient-0 & variables-0] sum-0
        [coefficient-1 & variables-1] sum-1]
    (concat [(* coefficient-0 coefficient-1)] variables-0 variables-1)))

(defn simplify-polynomial [polynomial]
  (cond
    (spec/valid? ::term polynomial) (simplify-term polynomial)
    (spec/valid? ::sum polynomial) (->> (rest polynomial)
                                        (map simplify-polynomial)
                                        (reduce add-sums ())
                                        (simplify-sum)
                                        (concat ['+]))
    (spec/valid? ::product polynomial) (->> (rest polynomial)
                                            (map simplify-polynomial)
                                            (map #(if (spec/valid? ::term %) [%] (rest %)))
                                            (reduce multiply-sums)
                                            (simplify-sum)
                                            (concat ['+]))))

(defn substitute [polynomial substitution]
  (walk/postwalk #(if (and (spec/valid? ::variable-power %) (contains? substitution (first %)))
                    (expt (substitution (first %)) (second %))
                    %)
                 polynomial))

(defn arithmetic-negate [polynomial]
  ['+ [1] ['* [-1] polynomial]])

(defn arithmetize [formula]
  (cond
    (spec/valid? ::literal formula)
    (case formula false [0] true [1] [1 [formula 1]])

    (spec/valid? ::negation formula)
    (arithmetic-negate (arithmetize (second formula)))

    (spec/valid? ::conjunction formula)
    (into ['*] (map arithmetize (rest formula)))

    (spec/valid? ::disjunction formula)
    (->> (rest formula)
         (map (comp arithmetic-negate arithmetize))
         (into ['*])
         (arithmetic-negate))

    (spec/valid? ::all formula)
    (let [[_ variable body] formula]
      ['*
       (arithmetize (walk/postwalk-replace {variable false} body))
       (arithmetize (walk/postwalk-replace {variable true} body))])

    (spec/valid? ::exists formula)
    (let [[_ variable body] formula]
      (arithmetic-negate
        ['*
         (arithmetic-negate (arithmetize (walk/postwalk-replace {variable false} body)))
         (arithmetic-negate (arithmetize (walk/postwalk-replace {variable true} body)))]))))

