(ns tnoc.ip
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.walk :as walk]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.string :as str]))

(load "ip_spec")

(defn- make-sum [polynomials]
  (cond (empty? polynomials) 0
        (= 1 (count polynomials)) (first polynomials)
        :else (concat ['+] polynomials)))

(defn- make-product [polynomials]
  (cond (empty? polynomials) 1
        (= 1 (count polynomials)) (first polynomials)
        :else (concat ['*] polynomials)))

(defn- separate-coefficient [polynomials]
  "Takes a sequence of polynomials and returns a tuple containing the constant coefficient as well
  as a sequence of the remaining factors."
  (let [{numbers     true
         rem-factors false} (group-by number? polynomials)]
    [(reduce * numbers) (into () rem-factors)]))

(defn- merge-factors [polynomials]
  "Takes a sequence of polynomials - presumed to be factors of a product - and merges repeated
  occurrences by adding the exponents."
  (let [[coefficient factors] (separate-coefficient polynomials)
        merged-factors (->> factors
                            (map #(if (spec/valid? ::exponentiation %) {(nth % 1) (nth % 2)} {% 1}))
                            (apply merge-with +)
                            (map #(if (= 1 (second %)) (first %) (concat ['**] %))))]
    (cond
      (zero? coefficient) 0
      (= 1 coefficient) (make-product merged-factors)
      :else (if (empty? merged-factors)
              coefficient
              (concat ['* coefficient] merged-factors)))))

(defn- merge-terms [polynomials]
  "Takes a sequence of polynomials - presumed to be terms of a sum - and merges repeated
  occurrences by adding the constant coefficients."
  (->> polynomials
       (map #(apply hash-map (reverse (separate-coefficient (if (spec/valid? ::product %) (rest %) [%])))))
       (apply merge-with +)
       (filter (comp not zero? second))
       (map (fn [[rem-factors coefficient]] (merge-factors (conj rem-factors coefficient))))
       (make-sum)))

(defn- add [polynomial-1 polynomial-2]
  (letfn [(get-terms [p] (if (spec/valid? ::sum p) (rest p) [p]))]
    (merge-terms (concat (get-terms polynomial-1) (get-terms polynomial-2)))))

(defn- multiply [polynomial-1 polynomial-2 distributive?]
  (cond
    (and distributive? (spec/valid? ::sum polynomial-1))
    (reduce add 0 (map #(multiply % polynomial-2 distributive?) (rest polynomial-1)))

    (and distributive? (spec/valid? ::sum polynomial-2))
    (reduce add 0 (map #(multiply polynomial-1 % distributive?) (rest polynomial-2)))

    :else (letfn [(get-factors [p] (if (spec/valid? ::product p) (rest p) [p]))]
            (merge-factors (concat (get-factors polynomial-1) (get-factors polynomial-2))))))

(defn simplify
  "Simplifies a polynomial by merging factors and terms. Also multiplies out sums appearing in
  products if true is passed in as the second argument."
  ([polynomial] (simplify polynomial true))
  ([polynomial distributive?]
   (cond
     (spec/valid? ::alg-literal polynomial) polynomial

     (spec/valid? ::exponentiation polynomial) (let [[_ base exponent] polynomial
                                                     simplified-base (simplify base distributive?)]
                                                 (if (number? simplified-base)
                                                   (expt simplified-base exponent)
                                                   (list '** simplified-base exponent)))

     (spec/valid? ::product polynomial) (->> (map #(simplify % distributive?) (rest polynomial))
                                             (reduce #(multiply %1 %2 distributive?) 1))

     (spec/valid? ::sum polynomial) (->> (map #(simplify % distributive?) (rest polynomial))
                                         (reduce add 0)))))

(def substitute #(walk/postwalk-replace %2 %1))

(defn- arithmetic-negate [polynomial]
  "Performs arithmetic negation by converting P(x) into 1-P(x)."
  (make-sum [1 (make-product [-1 polynomial])]))

(defn- arithmetic-forall [polynomial variable]
  "Performs an arithmetic for-all by converting P(x) into P(0)*P(1)."
  (make-product [(substitute polynomial {variable 0})
                 (substitute polynomial {variable 1})]))

(defn- arithmetic-exists [polynomial variable]
  "Performs an arithmetic there-exists by converting P(x) into 1-(1-P(0))*(1-P(1))."
  (arithmetic-negate (make-product [(arithmetic-negate (substitute polynomial {variable 0}))
                                    (arithmetic-negate (substitute polynomial {variable 1}))])))

(defn- arithmetic-reduced [polynomial variable]
  "Performs an arithmetic reduction by converting P(x) into P(0)+(P(1)-P(0))*x."
  (let [subst-0 (substitute polynomial {variable 0})
        subst-1 (substitute polynomial {variable 1})]
    (make-sum [subst-0 (make-product [variable (make-sum [subst-1 (make-product [-1 subst-0])])])])))

(defn arithmetize [formula]
  "Turns a formula of boolean algebra into a polynomial. Any satisfying assignment of the boolean
  formula will correspond to an assignment for which the polynomial evaluates to 1, provided that
  false is translated to 0 and true to 1. Similarly, non-satisfying assignments will correspond to
  assignments of 0 and 1 for which the polynomial evaluates to 0."
  (cond
    (spec/valid? ::bool-literal formula)
    (case formula false 0 true 1 formula)

    (spec/valid? ::negation formula)
    (arithmetic-negate (arithmetize (second formula)))

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

(defn- unquantified? [formula] (not (spec/valid? ::quantified formula)))

(defn arithmetize-simplifying [formula]
  "Equivalent to calling (simplify (arithmetize formula)), but a lot faster."
  (cond
    (unquantified? formula) (simplify (arithmetize formula))

    (spec/valid? ::forall formula)
    (let [[_ variable body] formula]
      (simplify (arithmetic-forall (arithmetize-simplifying body) variable)))

    (spec/valid? ::exists formula)
    (let [[_ variable body] formula]
      (simplify (arithmetic-exists (arithmetize-simplifying body) variable)))

    (spec/valid? ::reduced formula)
    (let [[_ variable body] formula]
      (simplify (arithmetic-reduced (arithmetize-simplifying body) variable)))))

(defn- merlin [formula substitutions]
  "Produces a fully simplified arithmetization of the given formula, using the given substitutions
  for any free variables. If the given formula is quantified, this function will also arithmetize the
  body of the given formula and return that as a second element in a tuple.
  Otherwise the second return value is nil."
  [(simplify (substitute (arithmetize-simplifying formula) substitutions))
   (if (spec/valid? ::quantified formula)
     (let [[_ variable body] formula]
       (-> (arithmetize-simplifying body)
           (substitute (dissoc substitutions variable))
           (simplify))))])

(defn- arthur [formula claim proof substitutions]
  "Assumes that the given proof is the correct arithmetization of the body of the formula. Using that,
   as well as the substitutions for any free variables, verifies that the claim is indeed the correct
   arithmetization of the entire formula."
  (if (unquantified? formula)
    (= claim (simplify (substitute (arithmetize formula) substitutions)))
    (= claim (simplify (let [[quantifier variable _] formula]
                         (case quantifier
                           A (arithmetic-forall proof variable)
                           E (arithmetic-exists proof variable)
                           R (simplify (substitute (arithmetic-reduced proof variable) substitutions))))))))

(defn free-variables [formula]
  "Returns a set of a all free variables in the given quantified boolean formula."
  (if (unquantified? formula)
    (into #{} (filter keyword? (flatten formula)))
    (let [[quantifier variable body] formula]
      (if (= 'R quantifier)
        (recur body)
        (disj (free-variables body) variable)))))

(defn- interact-once [formula substitutions]
  "Produces a map containing the given formula, Merlin's claim and proof, as well as a boolean
  indicating whether Arthur was able to verify Merlin's claim."
  (if (every? (into #{} (keys substitutions)) (free-variables formula))
    (let [[claim proof] (merlin formula substitutions)
          verified? (arthur formula claim proof substitutions)]
      {:formula       formula
       :substitutions substitutions
       :claim         claim
       :proof         proof
       :verified?     verified?})))

(defn interact [formula test-ints]
  "Produces a list of tuples containing the sub-formula that's currently being verified, Merlin's
  claim and proof, as well as a boolean indicating whether Arthur was able to verify Merlin's claim."
  (loop [formula formula
         substitutions {}
         [test-int & rem-test-ints :as test-ints] test-ints
         interactions []]
    (let [unassigned (first (remove (set (keys substitutions)) (free-variables formula)))]
      (if unassigned
        (if test-int
          (recur formula (assoc substitutions unassigned test-int) rem-test-ints interactions)
          interactions)
        (if (unquantified? formula)
          (conj interactions (interact-once formula substitutions))
          (recur (nth formula 2)
                 substitutions
                 test-ints
                 (conj interactions (interact-once formula substitutions))))))))

(defn pprint-polynomial [polynomial]
  "Converts a polynomial into a more readable string."
  (cond
    (number? polynomial) (str polynomial)
    (keyword? polynomial) (name polynomial)

    (spec/valid? ::exponentiation polynomial)
    (str (pprint-polynomial (nth polynomial 1)) "^" (nth polynomial 2))

    (spec/valid? ::product polynomial)
    (let [[coefficient factors] (separate-coefficient (rest polynomial))
          pprinted-coefficient (if (and (= -1 coefficient) (seq factors))
                                 "-"
                                 (str coefficient))
          pprinted-factors (->> factors
                                (map #(if (spec/valid? ::sum %) (str "(" (pprint-polynomial %) ")") (pprint-polynomial %)))
                                (apply str))]
      (str pprinted-coefficient pprinted-factors))

    (spec/valid? ::sum polynomial)
    (->> (map pprint-polynomial (rest polynomial))
         (reduce #(if (str/starts-with? %2 "-") (str %1 " - " (subs %2 1)) (str %1 " + " %2))))))

(defn pprint-interaction [{formula :formula substitutions :substitutions claim :claim proof :proof verified? :verified?}]
  "Pretty-prints both the claim and proof in the given interaction."
  {:formula       formula
   :substitutions substitutions
   :claim         (pprint-polynomial claim)
   :proof         (pprint-polynomial proof)
   :verified?     verified?})

(defn interact-manually [formula]
  "Starts an interactive proof in the console where the test integers can be passed in and used for
  the proof protocol one at a time."
  (loop [formula formula
         substitutions {}]
    (let [unassigned (first (remove (set (keys substitutions)) (free-variables formula)))]
      (if unassigned
        (let [line (do (println (str unassigned " needs an assignment.")) (read-line))]
          (if (re-matches #"[0-9]+" line)
            (recur formula (assoc substitutions unassigned (read-string line)))))
        (if (unquantified? formula)
          (println (pprint-interaction (interact-once formula substitutions)))
          (do (println (pprint-interaction (interact-once formula substitutions)))
              (recur (nth formula 2) substitutions)))))))

(defn remove-reductions [formula]
  "Removes all reduction operators from a quantified boolean formula."
  (if (unquantified? formula)
    formula
    (let [[quantifier variable body] formula]
      (if (= 'R quantifier)
        (recur body)
        (list quantifier variable (remove-reductions body))))))

(defn add-reductions [formula]
  "Inserts the reduction operator as a appropriate in a quantified boolean formula."
  (letfn [(impl [formula]
            (if (unquantified? formula)
              (reduce #(list 'R %2 %1) formula (reverse (sort (free-variables formula))))
              (let [[quantifier variable body] formula]
                (reduce #(list 'R %2 %1)
                        (list quantifier variable (impl body))
                        (reverse (sort (free-variables formula)))))))]
    (impl (remove-reductions formula))))
