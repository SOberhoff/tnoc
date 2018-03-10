(ns tnoc.lambda
  (:require [clojure.zip :as zip]
            [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as sgen]
            [com.rpl.specter :as spt]
            [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [clojure.string :as str]))

(load "lambda_spec")

(defn abstraction? [form] (spec/valid? ::abstraction form))

(defn- prewalk-form [form f]
  "Walks a form in pre-order and transforms each visited sub-form using f. f is expected to take 2
  arguments. First the current sub-form and second a set of variables that are bound in the current
  sub-form."
  (letfn [(impl [sub-form bound-variables]
            (let [transformed (f sub-form bound-variables)]
              (cond
                (abstraction? transformed)
                (spt/transform [(spt/nthpath 2)]
                               #(impl % (into bound-variables (second transformed)))
                               transformed)

                (seq? transformed) (map #(impl % bound-variables) transformed)
                :else transformed)))]
    (impl form #{})))

(defn- postwalk-form [form f]
  "Walks a form in post-order and transforms each visited sub-form using f. f is expected to take 2
  arguments. First the current sub-form and second a set of variables that are bound in the current
  sub-form."
  (letfn [(impl [sub-form bound-variables]
            (cond
              (abstraction? sub-form)
              (-> (spt/transform [(spt/nthpath 2)]
                                 #(impl % (into bound-variables (second sub-form)))
                                 sub-form)
                  (f bound-variables))

              (seq? sub-form) (f (map #(impl % bound-variables) sub-form) bound-variables)
              :else (f sub-form bound-variables)))]
    (impl form #{})))

(defn- substitute [body symbol argument]
  "Replaces every non-shadowed occurrence of `symbol` in `body` with `argument`."
  (prewalk-form body #(if (and (symbol? %1) (= symbol %1) (not (%2 symbol))) argument %1)))

(defn- fuzz-parameter [[_ params body :as abstraction] param]
  "If `param` collides with another parameter in `params`,
   the collision is mended by replacing the latter with a generated symbol."
  (if-not (some #{param} params)
    abstraction
    (let [new-param (gensym)
          new-params (mapv #(if (= param %) new-param %) params)]
      (list 'fn new-params (substitute body param new-param)))))

(defn- free-variables [form]
  ((fn recursive-find [form bound-variables]
     (match form
            (([_ params body] :seq) :guard abstraction?) (recursive-find body (into bound-variables params))
            (nested-form :guard #(spec/valid? ::nested-form %)) (apply set/union (map #(recursive-find % bound-variables) nested-form))
            (free-variable :guard (every-pred symbol? (comp not bound-variables))) #{free-variable}
            :else #{}))
    form #{}))

(defn- apply-abstraction [abstraction operand]
  (let [[_ params body] (reduce fuzz-parameter abstraction (free-variables operand))]
    (if (= 1 (count params))
      (substitute body (first params) operand)
      (list 'fn (into [] (rest params)) (substitute body (first params) operand)))))

(defn church [n]
  "Converts a natural number to the corresponding church numeral."
  `(~'fn ~['f 'x] ~(nth (iterate #(seq ['f %]) 'x) n)))

(defn- final-apply? [form]
  "Tests if `form` is of the form '(f x) where f is an abstraction."
  (and (seq? form) (abstraction? (first form)) (= 2 (count form))))

(defn- non-final-apply? [form]
  "Tests if `form` is of the form '(f x & more) where f is an abstraction."
  (and (seq? form) (abstraction? (first form)) (< 2 (count form))))

(defn- get-symbol [symbol]
  (if (symbol? symbol) (some-> (resolve symbol) (var-get) (#(if (seq? %) %)))))

(def ^:private first-reducible-path
  (spt/comp-paths (spt/filterer #(or (get-symbol %) (integer? %) (spec/valid? ::nested-form %)))
                  spt/FIRST))

(def normalize-once
  (memoize
    (fn [form]
      (cond
        (final-apply? form) (apply-abstraction (first form) (second form))

        (non-final-apply? form) (conj (drop 2 form) (apply-abstraction (first form) (second form)))

        (get-symbol form) (get-symbol form)

        (integer? form) (church form)

        (spec/valid? ::nested-form form) (spt/transform first-reducible-path normalize-once form)

        :else form))))

(def normalize
  (memoize
    (fn [form]
      (cond
        (final-apply? form) (recur (apply-abstraction (first form) (second form)))

        (non-final-apply? form) (recur (conj (drop 2 form) (apply-abstraction (first form) (second form))))

        (get-symbol form) (recur (get-symbol form))

        (integer? form) (church form)

        (spec/valid? ::nested-form form)
        (let [normalized (spt/transform first-reducible-path normalize form)]
          (if (= form normalized) form (recur normalized)))

        :else form))))


(defn normalized? [form] (= form (normalize-once form)))

(defn- merge-abstractions [[_ outer-params [_ inner-params body]]]
  "Simplifies a nested abstraction of the form '(fn [x & more] (fn [y & more] ...)) into '(fn [x y & more] ...)"
  (let [params (-> (mapv #(if (some #{%} inner-params) (gensym) %) outer-params)
                   (into inner-params))]
    (list 'fn params body)))

(defn normalize-simplified-once [form]
  (if (abstraction? form)
    (if (abstraction? (nth form 2))
      (merge-abstractions form)
      (spt/transform [(spt/nthpath 2)] normalize-simplified-once form))
    (normalize-once form)))

(defn normalize-simplified [form]
  "Like `normal-form` but also tries to normalize bodies of abstractions."
  (let [normalized (normalize form)]
    (if (abstraction? normalized)
      (let [with-normalized-body (spt/transform [(spt/nthpath 2)] normalize-simplified normalized)]
        (if (abstraction? (nth with-normalized-body 2))
          (merge-abstractions with-normalized-body)
          with-normalized-body))
      normalized)))

(defn normalize-simplified? [form] (= form (normalize-simplified-once form)))

(defn normalize-reductions [form]
  "Produces a lazy seq of reductions on the path to normal form."
  (reductions #(if (normalized? %2) (reduced %2) %2) (iterate #(normalize %) form)))

(defn normalize-simplified-reductions [form]
  "Like `normal-form-reductions` but also tries to normalize bodies of abstractions."
  (reductions #(if (normalize-simplified? %2) (reduced %2) %2) (iterate #(normalize-simplified-once %) form)))

(defn println-reductions [form]
  "Like `normal-form-reductions` but printlns the reductions with line numbers instead."
  (doseq [[index reduction] (map-indexed vector (normalize-reductions form))]
    (println (str index ": " (pr-str reduction)))))

(defn println-reductions-simplified [form]
  "Like `normal-form-reductions-simplified` but printlns the reductions with line numbers instead."
  (doseq [[index reduction] (map-indexed vector (normalize-simplified-reductions form))]
    (println (str index ": " (pr-str reduction)))))

(defn unchurch [church-numeral]
  "Inverse of `church`. Will also attempt to invert church numerals not in the standard form '(fn [f x] (f (f (...f x)...)))"
  (eval (list (normalize-simplified church-numeral) inc 0)))

(defn resolve-references [form]
  "Replaces all references with their definitions and integers with their Church numerals."
  (walk/prewalk #(cond
                   (get-symbol %) (get-symbol %)
                   (integer? %) (church %)
                   :else %) form))

; Predefined forms. Extensive use of namespace-qualification is made to minimize issues caused by attempting
; resolution in the wrong namespace.

(def I '(fn [x] x))

(def T '(fn [x y] x))

(def F '(fn [x y] y))

(def NOT '(fn [p x y] (p y x)))

(def AND '(fn [p q] (p q p)))

(def OR '(fn [p q] (p p q)))

(def SUCC '(fn [m f x] (f (m f x))))

(def ADD '(fn [n m f x] (n f (m f x))))

(def ADD' `(~'fn [~'n] (~'n SUCC)))

(def MULT '(fn [n m f x] (n (m f) x)))

(def MULT' '(fn [n m f] (n (m f))))

(def EXP '(fn [n m] (m n)))

(def PAIR '(fn [x y f] (f x y)))

(def FIRST `(~'fn [~'p] (~'p T)))

(def SECOND `(~'fn [~'p] (~'p F)))

(def J|J+1 `(~'fn [~'p] (PAIR (SECOND ~'p) (SUCC (SECOND ~'p)))))

(def PRED `(~'fn [~'n] (FIRST ((~'n J|J+1) (PAIR 0 0)))))

(def COMP '(fn [f g x] (f (g x))))

(def ZERO? `(~'fn [~'n] (~'n (~'fn [~'x] F) T)))

(def Y '(fn [f] ((fn [x] (f (x x))) (fn [x] (f (x x))))))

; The Mockingbird. Thanks Raymond Smullyan.
(def M '(fn [x] (x x)))

(def Y' `(~'fn [~'f] (M (COMP ~'f M))))

(def FAC `(Y (~'fn [~'fac ~'n] ((ZERO? ~'n) 1 (MULT ~'n (~'fac (PRED ~'n)))))))

(def FIB `(Y (~'fn [~'fib ~'n] ((ZERO? ~'n) 1 ((ZERO? (PRED ~'n)) 1 (ADD (~'fib (PRED ~'n)) (~'fib (PRED (PRED ~'n)))))))))
