(ns tnoc.primitive-recursion
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [tnoc.primitive-recursion :refer :all]
            [clojure.tools.trace :refer [trace]]
            [com.rpl.specter :as spt]))

(load "primitive_recursion_spec")

(defn S-to-inc [body] (walk/prewalk-replace {'S 'inc} body))

(defn make-reducing-fn-body [name recur-param current index recur-body]
  (walk/prewalk #(cond (= % recur-param) index
                       (and (seq? %) (= name (first %))) current
                       :else %)
                recur-body))

(defn make-reducing-fn [name modified-recur-params recur-body]
  (let [recur-param (last modified-recur-params)
        current (gensym)
        index (gensym)
        reducing-fn-body (make-reducing-fn-body name recur-param current index recur-body)
        base-call `(~name ~@(spt/setval [spt/LAST] 0 modified-recur-params))
        reducing-fn-sym (gensym)]
    (eval `(def ~reducing-fn-sym
             (memoize
               (fn ~modified-recur-params
                 (memoize (fn [~current ~index] ~reducing-fn-body))))))
    `(reduce (apply ~reducing-fn-sym ~modified-recur-params) ~base-call (range ~recur-param))))

(defn modify-recur [name recur-params recur-body]
  (if (= '_ (last recur-params))
    [recur-params (S-to-inc recur-body)]
    (let [modified-recur-params (spt/transform [spt/LAST] second recur-params)]
      [modified-recur-params (make-reducing-fn name modified-recur-params (S-to-inc recur-body))])))

(defmacro primrec
  ([name params body]
   `(defn ~(vary-meta name assoc :primrec true) ~params ~(S-to-inc body)))
  ([name base-params base-body recur-params recur-body]
   (let [gen-params (into [] (repeatedly (count base-params) gensym))
         [modified-recur-params modified-recur-body] (modify-recur name recur-params recur-body)]
     `(defn ~(vary-meta name assoc :primrec true) ~gen-params
        (match ~gen-params
               ~base-params ~(walk/prewalk #(if (#{'S} %) 'inc %) base-body)
               ~modified-recur-params ~modified-recur-body)))))

(primrec add
         [x 0] x
         [x (S y)] (S (add x y)))

(macroexpand-1 '(primrec add
                         [x 0] x
                         [x (S y)] (S (add x y))))

(primrec mult
         [x 0] 0
         [x (S y)] (add (mult x y) x))

(primrec exp
         [x 0] (S 0)
         [x (S y)] (mult (exp x y) x))

(primrec pred
         [0] 0
         [(S x)] x)

(primrec sub
         [x 0] x
         [x (S y)] (pred (sub x y)))

(primrec pos
         [0] 0
         [_] (S 0))

(primrec leq
         [x y] (pos (sub (S y) x)))

(primrec Z?
         [0] (S 0)
         [_] 0)

(primrec even
         [0] (S 0)
         [(S x)] (Z? (even x)))

(primrec div2
         [0] 0
         [(S x)] (add (even (S x)) (div2 x)))

(primrec lg-rec
         [_ 0] 0
         [x (S y)] (add (mult (leq (exp (S (S 0)) (S y)) x) (S y))
                        (mult (even (leq (exp (S (S 0)) (S y)) x)) (lg-rec x y))))

(primrec lg [x] (lg-rec x x))
