(ns tnoc.primitive-recursion
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [com.rpl.specter :as spt]))

(load "primitive_recursion_spec")

(defn S-to-inc [body] (walk/prewalk-replace {'S 'inc} body))

(defn modify-recur [recur-params recur-body]
  (if (#{'_} (last recur-params))
    [recur-params (S-to-inc recur-body)]
    (let [modified-recur-params (spt/transform [spt/LAST] second recur-params)
          recur-param (second (last recur-params))
          modified-recur-body (walk/postwalk-replace {recur-param (list 'dec recur-param)}
                                                     (S-to-inc recur-body))]
      [modified-recur-params modified-recur-body])))

(defmacro primrec
  ([name params body]
   `(defn ~(vary-meta name assoc :primrec true) ~params ~(S-to-inc body)))
  ([name base-params base-body recur-params recur-body]
   (let [gen-params (into [] (repeatedly (count base-params) gensym))]
     `(defn ~(vary-meta name assoc :primrec true) ~gen-params
        (match ~gen-params
               ~base-params ~(S-to-inc base-body)
               ~@(modify-recur recur-params recur-body))))))

(primrec add
         [x 0] x
         [x (S y)] (S (add x y)))

(macroexpand-1 '(primrec add
                         [x 0] x
                         [x (S y)] (S (add x y))))

(primrec mult
         [x 0] 0
         [x (S y)] (add x (mult x y)))

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

(primrec grt
         [x y] (pos (sub x y)))

(primrec Z?
         [0] (S 0)
         [_] 0)

(primrec select
         [x y 0] x
         [x y _] y)

(primrec even
         [0] (S 0)
         [(S x)] (Z? (even x)))

(primrec div2
         [0] 0
         [(S x)] (add (even (S x)) (div2 x)))

(primrec lg-rec
         [x 0] (S 0)
         [x (S y)] (select (lg-rec x y) (sub x (S (S y))) (grt (exp (S (S 0)) (sub x (S y))) x)))

(primrec lg [x] (lg-rec x x))
