(ns tnoc.ip
  (:require [clojure.spec.alpha :as spec]
            [clojure.walk :as walk]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.string :as str]
            [com.rpl.specter :as spt]))

(defrecord Sum [polynomials])

(defrecord Product [polynomials])

(load "ip_spec")

(defn group-by-polynomials [polynomials]
  (group-by #(first (spec/conform ::polynomial %)) polynomials))

(defn get-degree [[coefficient variables]]
  (if (zero? coefficient)
    0
    (reduce + (map second variables))))

(defn sort-polynomials [polynomials]
  (let [{terms    :term
         sums     :Sum
         products :Product} (group-by-polynomials polynomials)]
    (concat (reverse (sort-by get-degree terms)) sums products)))

(defn make-sum [polynomials]
  (let [filtered-and-sorted (sort-polynomials (filter #(not= [0 {}] %) polynomials))]
    (case (count filtered-and-sorted)
      0 [0 {}]
      1 (first filtered-and-sorted)
      (->Sum filtered-and-sorted))))

(defn make-product [polynomials]
  (let [filtered-and-sorted (sort-polynomials (filter #(not= [1 {}] %) polynomials))]
    (case (count filtered-and-sorted)
      0 [1 {}]
      1 (first filtered-and-sorted)
      (->Product filtered-and-sorted))))

(defn simplify-term [[coefficient variables]]
  (if (zero? coefficient)
    [0 {}]
    [coefficient (spt/setval [spt/MAP-VALS zero?] spt/NONE variables)]))

(defn multiply-terms [[coefficient-0 variables-0] [coefficient-1 variables-1]]
  (simplify-term [(* coefficient-0 coefficient-1) (merge-with + variables-0 variables-1)]))

(defn add [{polynomials :polynomials}]
  (let [{terms    :term
         sums     :Sum
         products :Product} (group-by-polynomials polynomials)
        grouped-terms (->> (group-by second terms)
                           (spt/transform [spt/MAP-VALS] #(reduce + (map first %)))
                           (map #(vector (second %) (first %))))]
    (->> (concat grouped-terms (mapcat :polynomials sums) products)
         (spt/setval [spt/ALL #(spec/valid? ::term %) (comp zero? first)] spt/NONE)
         (make-sum))))

(defn multiply [{polynomials :polynomials} distributive?]
  (let [{terms    :term
         sums     :Sum
         products :Product} (group-by-polynomials polynomials)
        term (reduce multiply-terms [1 {}] terms)]
    (if (= [0 {}] term)
      term
      (if distributive?
        (->> (map :polynomials sums)
             (reduce
               #(for [multiplicand-0 %1 multiplicand-1 %2]
                  (if (every? (partial spec/valid? ::term) [multiplicand-0 multiplicand-1])
                    (multiply-terms multiplicand-0 multiplicand-1)
                    (->Product [multiplicand-0 multiplicand-1])))
               [term])
             (make-sum)
             (conj (mapcat :polynomials products))
             (make-product))
        (->> (concat [term] sums (mapcat :polynomials products))
             (make-product))))))

(defn simplify-polynomial [polynomial distributive?]
  (cond
    (spec/valid? ::term polynomial)
    (simplify-term polynomial)

    (spec/valid? ::Sum polynomial)
    (->> polynomial
         (spt/transform [:polynomials spt/ALL] #(simplify-polynomial % distributive?))
         (add))

    (spec/valid? ::Product polynomial)
    (let [multiplicands-simplified (spt/transform [:polynomials spt/ALL]
                                                  #(simplify-polynomial % distributive?)
                                                  polynomial)
          multiplied (multiply multiplicands-simplified distributive?)]
      (if (spec/valid? ::Sum multiplied)
        (add multiplied)
        multiplied))))

(defn substitute [polynomial substitution]
  (walk/prewalk
    #(if (spec/valid? ::term %)
       (reduce (fn [[coefficient variables] [variable value]]
                 [(* coefficient (expt value (variables variable 0)))
                  (dissoc variables variable)])
               % substitution)
       %)
    polynomial))

(defn arithmetic-negate [polynomial]
  (->Sum [[1 {}] (->Product [[-1 {}] polynomial])]))

(defn arithmetize [formula]
  (cond
    (spec/valid? ::literal formula)
    (case formula false [0 {}] true [1 {}] [1 {formula 1}])

    (spec/valid? ::negation formula)
    (let [to-negate (second formula)]
      (if (spec/valid? ::negation to-negate)
        (arithmetize (second to-negate))
        (arithmetic-negate (arithmetize to-negate))))

    (spec/valid? ::conjunction formula)
    (make-product (map arithmetize (rest formula)))

    (spec/valid? ::disjunction formula)
    (->> (rest formula)
         (map #(arithmetize ['not %]))
         (make-product)
         (arithmetic-negate))

    (spec/valid? ::all formula)
    (let [[_ variable body] formula]
      (->Product [(arithmetize (walk/postwalk-replace {variable false} body))
                  (arithmetize (walk/postwalk-replace {variable true} body))]))

    (spec/valid? ::exists formula)
    (let [[_ variable body] formula]
      (arithmetic-negate
        (->Product
          [(arithmetic-negate (arithmetize (walk/postwalk-replace {variable false} body)))
           (arithmetic-negate (arithmetize (walk/postwalk-replace {variable true} body)))])))))

(defn serialize-term [[coefficient variables]]
  (if (empty? variables)
    (str coefficient)
    (str (case coefficient 1 "" -1 "-" coefficient)
         (->> variables
              (map (fn [[base power]] (str (name base) (if (= 1 power) "" (str "^" power)))))
              (apply str)))))

(defn serialize [polynomial]
  (cond
    (spec/valid? ::term polynomial)
    (serialize-term polynomial)

    (spec/valid? ::Sum polynomial)
    (reduce #(if (str/starts-with? %2 "-")
               (str %1 " - " (subs %2 1))
               (str %1 " + " (str %2)))
            (map serialize (:polynomials polynomial)))

    (spec/valid? ::Product polynomial)
    (let [{terms    :term
           sums     :Sum
           products :Product} (group-by-polynomials (:polynomials polynomial))
          term (reduce multiply-terms [1 {}] terms)
          serialized-sums-products (->> (concat sums products)
                                        (map #(str \( (serialize %) \)))
                                        (apply str))]
      (case term
        [0 {}] "0"
        [1 {}] serialized-sums-products
        [-1 {}] (str \- serialized-sums-products)
        (str (serialize-term term) serialized-sums-products)))))
