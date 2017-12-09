(ns tnoc.ip
  (:require [tnoc.utils :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.walk :as walk]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.string :as str]
            [com.rpl.specter :as spt]
            [clojure.set :as set]))

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

(defn simplify
  ([polynomial] (simplify polynomial true))
  ([polynomial distributive?]
   (cond
     (spec/valid? ::term polynomial)
     (simplify-term polynomial)

     (spec/valid? ::Sum polynomial)
     (->> polynomial
          (spt/transform [:polynomials spt/ALL] #(simplify % distributive?))
          (spt/transform [:polynomials] (partial mapcat #(if (spec/valid? ::Sum %) (:polynomials %) [%])))
          (add))

     (spec/valid? ::Product polynomial)
     (let [multiplicands-simplified (spt/transform [:polynomials spt/ALL]
                                                   #(simplify % distributive?)
                                                   polynomial)
           multiplied (multiply multiplicands-simplified distributive?)]
       (if (spec/valid? ::Sum multiplied)
         (add multiplied)
         multiplied)))))

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

(defn arithmetic-forall [polynomial variable]
  (->Product [(substitute polynomial {variable 0})
              (substitute polynomial {variable 1})]))

(defn arithmetic-exists [polynomial variable]
  (arithmetic-negate (->Product [(arithmetic-negate (substitute polynomial {variable 0}))
                                 (arithmetic-negate (substitute polynomial {variable 1}))])))

(defn arithmetic-reduced [polynomial variable]
  (let [subst-0 (substitute polynomial {variable 0})
        subst-1 (substitute polynomial {variable 1})]
    (->Sum [subst-0
            (->Product [[1 {variable 1}]
                        (->Sum [subst-1 (->Product [[-1 {}] subst-0])])])])))

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

    (spec/valid? ::forall formula)
    (let [[_ variable body] formula] (arithmetic-forall (arithmetize body) variable))

    (spec/valid? ::exists formula)
    (let [[_ variable body] formula] (arithmetic-exists (arithmetize body) variable))

    (spec/valid? ::reduced formula)
    (let [[_ variable body] formula] (arithmetic-reduced (arithmetize body) variable))))

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
          serialized-sums (apply str (map #(str \( (serialize %) \)) sums))
          serialized-products (apply str (map #(str (serialize %)) products))
          serialized-sums-products (str serialized-sums serialized-products)]
      (case term
        [0 {}] "0"
        [1 {}] (if (empty? serialized-sums-products) "1" serialized-sums-products)
        [-1 {}] (if (empty? serialized-sums-products) "-1" (str \- serialized-sums-products))
        (str (serialize-term term) serialized-sums-products)))))

(defn unquantified? [formula] (not (spec/valid? ::quantified formula)))

(defn merlin [formula substitutions]
  (if (unquantified? formula)
    [(simplify (substitute (arithmetize formula) substitutions)) nil]
    [(simplify (substitute (arithmetize formula) substitutions))
     (let [[_ variable body] formula]
       (-> (arithmetize body)
           (substitute (dissoc substitutions variable))
           (simplify)))]))

(defn arthur [formula claim proof substitutions]
  (if (unquantified? formula)
    (= claim (simplify (substitute (arithmetize formula) substitutions)))
    (= claim (simplify (let [[quantifier variable _] formula]
                         (case quantifier
                           A (arithmetic-forall proof variable)
                           E (arithmetic-exists proof variable)
                           R (simplify (substitute (arithmetic-reduced proof variable) substitutions))))))))

(defn interact [formula test-ints]
  (letfn [(impl [formula test-ints substitutions]
            (if (unquantified? formula)
              (let [[claim proof] (merlin formula substitutions)
                    verified? (arthur formula claim proof substitutions)]
                [[nil nil claim proof verified?]])
              (let [[quantifier variable body] formula
                    [test-int & rem-test-ints] test-ints
                    [claim proof] (merlin formula substitutions)
                    verified? (arthur formula claim proof substitutions)]
                (lazy-cat [[quantifier variable claim proof verified?]]
                          (cond
                            (= 'R quantifier)
                            (impl body test-ints substitutions)

                            test-int
                            (impl body rem-test-ints (assoc substitutions variable test-int))

                            :else nil)))))]
    (impl formula test-ints {})))

(defn interact-serialized [formula test-ints]
  (->> (interact formula test-ints)
       (map (fn [[quantifier variable claim proof verified?]]
              [quantifier variable (serialize claim) (serialize proof) verified?]))))

(defn free-variables [formula]
  (if (unquantified? formula)
    (into #{} (filter keyword? (flatten formula)))
    (let [[quantifier variable body] formula]
      (if (= 'R quantifier)
        (recur body)
        (disj (free-variables body) variable)))))

(defn remove-reductions [formula]
  (if (unquantified? formula)
    formula
    (let [[quantifier variable body] formula]
      (if (= 'R quantifier)
        (recur body)
        (list quantifier variable (remove-reductions body))))))

(defn add-reductions [formula]
  (letfn [(impl [formula]
            (if (unquantified? formula)
              (reduce #(list 'R %2 %1) formula (reverse (sort (free-variables formula))))
              (let [[quantifier variable body] formula]
                (reduce #(list 'R %2 %1)
                        (list quantifier variable (impl body))
                        (reverse (sort (free-variables formula)))))))]
    (impl (remove-reductions formula))))
