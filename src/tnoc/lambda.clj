(ns tnoc.lambda
  (:require [clojure.zip :as zip]
            [clojure.spec :as spec]
            [clojure.spec.gen :as gen]
            [com.rpl.specter :as spt]
            [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [clojure.set :as set]))

(load "lambda_spec")

(defn- loc-descendant? [ancestor loc]
  (->> (iterate zip/up loc)
       (take-while some?)
       (some #{ancestor})))

(defn- next-skipping-children [loc]
  (->> (iterate zip/next loc)
       (filter #(or (not (loc-descendant? loc %)) (zip/end? %)))
       (first)))

(defn- next-symbol-occurrence [symbol loc]
  "Finds the next occurrence of `symbol` while passing over abstractions that shadow the symbol."
  (loop [loc loc]
    (let [node (zip/node loc)]
      (cond
        (zip/end? loc) nil

        (= symbol node) loc

        (and (spec/valid? ::abstraction node) (some #{symbol} (second node)))
        (recur (next-skipping-children loc))

        :else (recur (zip/next loc))))))

(defn- substitute [body symbol argument]
  "Replaces every non-shadowed occurrence of `symbol` in `body` with `argument`."
  (loop [edited-body (zip/seq-zip body)]
    (let [next (next-symbol-occurrence symbol edited-body)]
      (if next
        (recur (next-skipping-children (zip/replace next argument)))
        (zip/root edited-body)))))

(defn- fuzz-parameter [[_ params body :as abstraction] param]
  "If `param` collides with another parameter in `params`,
   the collision is mended by replacing the latter with a generated symbol."
  (if-not (some #{param} params)
    abstraction
    (let [new-param (gensym)
          new-params (mapv #(if (= param %) new-param %) params)]
      (list 'fn new-params (substitute body param new-param)))))

(defn- find-free-variables [form]
  ((fn recursive-find [form bound-variables]
     (match form
            (([_ params body] :seq) :guard #(spec/valid? ::abstraction %)) (recursive-find body (into bound-variables params))
            (nested-form :guard #(spec/valid? ::nested-form %)) (apply set/union (map #(recursive-find % bound-variables) nested-form))
            (free-variable :guard (every-pred symbol? (comp not bound-variables))) #{free-variable}
            :else #{}))
    form #{}))

(defn- apply-abstraction [abstraction operand]
  (let [[_ params body] (reduce fuzz-parameter abstraction (find-free-variables operand))]
    (if (= 1 (count params))
      (substitute body (first params) operand)
      (list 'fn (into [] (rest params)) (substitute body (first params) operand)))))

(defn church [n]
  "Converts a natural number to the corresponding church numeral."
  `(~'fn ~['f 'x] ~(nth (iterate #(seq ['f %]) 'x) n)))

(defn- final-apply? [form]
  "Tests if `form` is of the form '(f x) where f is an abstraction."
  (and (seq? form) (spec/valid? ::abstraction (first form)) (= 2 (count form))))

(defn- non-final-apply? [form]
  "Tests if `form` is of the form '(f x & more) where f is an abstraction."
  (and (seq? form) (spec/valid? ::abstraction (first form)) (< 2 (count form))))

(defn- mergeable-abstractions? [form]
  "Tests if form is a nested abstraction such as '(fn [x] (fn [y] ...)) which can be merged into '(fn [x y] ...)"
  (and (spec/valid? ::abstraction form) (spec/valid? ::abstraction (nth form 2))))

(defn- get-symbol [symbol]
  (if (symbol? symbol) (some-> (resolve symbol) (var-get) (#(if (seq? %) %)))))

(def ^:private first-reducible-path
  (spt/comp-paths (spt/filterer (some-fn get-symbol integer? (partial spec/valid? ::nested-form)))
                  spt/FIRST))

(def normalize
  "Takes a form to be normalized and a boolean flag `fully?` indicating whether only the next step is to be performed or
  if the final normal form is to be found."
  (memoize
    (fn [form fully?]
      (cond
        (final-apply? form)
        (cond-> (apply-abstraction (first form) (second form)) fully? (#(normalize % fully?)))

        (non-final-apply? form)
        (cond-> (conj (drop 2 form) (apply-abstraction (first form) (second form))) fully? (#(normalize % fully?)))

        (get-symbol form) (cond-> (get-symbol form) fully? (#(normalize % fully?)))

        (integer? form) (church form)

        (spec/valid? ::nested-form form)
        (#(if (and fully? (not= form %)) (normalize % fully?) %)
          (spt/compiled-transform first-reducible-path #(normalize % fully?) form))

        :else form))))

(defn normal-form [form] (normalize form true))

(defn normal-form? [form] (= form (normalize form false)))

(defn- merge-abstractions [[_ outer-params [_ inner-params body]]]
  "Simplifies a nested abstraction of the form '(fn [x & more] (fn [y & more] ...)) into '(fn [x y & more] ...)"
  (let [params (-> (mapv #(if (some #{%} inner-params) (gensym) %) outer-params)
                   (into inner-params))]
    (list 'fn params body)))

(defn normalize-simplify [form fully?]
  "Like `normalize` but also tries to normalize bodies of abstractions."
  (loop [next (zip/seq-zip (normalize form fully?))]
    (cond
      (and (not fully?) (not= form (zip/node next))) (zip/node next)

      (zip/end? next) (zip/root next)

      (mergeable-abstractions? (zip/node next))
      (let [replaced (zip/replace next (merge-abstractions (zip/node next)))]
        (if fully? (recur replaced) (zip/root replaced)))

      (spec/valid? ::abstraction (zip/node next))
      (let [[_ params body] (zip/node next)
            normalized-body (normalize body fully?)]
        (if (= body normalized-body)
          (recur (zip/next next))
          (let [replaced (zip/replace next (list 'fn params normalized-body))]
            (if fully? (recur replaced) (zip/root replaced)))))

      :else (recur (zip/next next)))))

(defn normal-form-simplified [form]
  "Like `normal-form` but also tries to normalize bodies of abstractions."
  (normalize-simplify form true))

(defn normal-form-simplified? [form] (= form (normalize-simplify form false)))

(defn normal-form-reductions [form]
  "Produces a lazy seq of reductions on the path to normal form."
  (reductions #(if (normal-form? %2) (reduced %2) %2) (iterate #(normalize % false) form)))

(defn normal-form-reductions-simplified [form]
  "Like `normal-form-reductions` but also tries to normalize bodies of abstractions."
  (reductions #(if (normal-form-simplified? %2) (reduced %2) %2) (iterate #(normalize-simplify % false) form)))

(defn println-reductions [form]
  "Like `normal-form-reductions` but printlns the reductions with line numbers instead."
  (doseq [[index reduction] (map-indexed vector (normal-form-reductions form))]
    (println (str index ": " (pr-str reduction)))))

(defn println-reductions-simplified [form]
  "Like `normal-form-reductions-simplified` but printlns the reductions with line numbers instead."
  (doseq [[index reduction] (map-indexed vector (normal-form-reductions-simplified form))]
    (println (str index ": " (pr-str reduction)))))

(defn unchurch [church-numeral]
  "Inverse of `church`. Will also attempt to invert church numerals not in the standard form '(fn [f x] (f (f (...f x)...)))"
  (eval (list (normal-form-simplified church-numeral) inc 0)))


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