(ns tnoc.primitive-recursion
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [clojure.spec :as spec]
            [com.rpl.specter :as spt]))

(load "primitive_recursion_spec")

(defn- S-to-inc [body] (walk/postwalk-replace {'S 'inc} body))

(defn- delay-force [body]
  "Makes the body-expression lazy by inserting `delay` and `force` as appropriate."
  (if-not (seq? body)
    `(force ~body)
    (->> body
         (walk/postwalk
           #(cond
              (and (seq? %) (#{'dec 'inc} (first %))) `(~(first %) (force ~(second %)))
              (seq? %) `(delay ~%)
              :else %))
         (list `force))))

(defn- S-to-dec [recur-params recur-body]
  "Converts a recursive case `[(S x)] (f x)` into `[x] (f (dec x))`."
  (let [last-param (last recur-params)
        recur-param (if (seq? last-param) (second last-param) last-param)]
    [(spt/setval [spt/LAST] recur-param recur-params)
     (walk/postwalk-replace {recur-param (list 'dec recur-param)} recur-body)]))

(defmacro primrec
  "Allows the definition of primitive recursive functions in a more succinct manner.
  `primrec` expects either a simple function definition via parameters and a body or
  a recursive function definition using a base case and a recursive case. In the latter
  case the last parameter of the base case must be `0` and the last parameter of the
  recursive case must be `(S x)` for some variable `x`.
  Any unused parameter except for the `0` in the base case of a recursive definition
  should be replaced by `_`.
  Furthermore `primrec` delays computation of arguments until actually needed. So
  `(f 0 (lifetime-of-the-universe))` will run much faster if `f` never uses its
   second argument. (Note that if `f` is recursive `(lifetime-of-the-universe)` must
   be forced to determine whether to apply the base case or recursive case.)"
  ([name params body]
   `(defn ~name ~params ~(delay-force (S-to-inc body))))
  ([name base-params base-body recur-params recur-body]
   (let [gen-params (into [] (repeatedly (count base-params) gensym))
         [modified-recur-params modified-recur-body] (S-to-dec recur-params recur-body)]
     `(defn ~name ~gen-params
        (match ~(spt/transform [spt/LAST] #(list `force %) gen-params)
               ~base-params ~(delay-force (S-to-inc base-body))
               ~modified-recur-params ~(delay-force (S-to-inc modified-recur-body)))))))

(primrec add
         [x 0] x
         [x (S y)] (S (add x y)))

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

(primrec ^{:doc "less than or equal"}
         leq
         [x y] (pos (sub (S y) x)))

(primrec ^{:doc "strictly greater"}
         grt
         [x y] (pos (sub x y)))

(primrec Z?
         [0] (S 0)
         [_] 0)

(primrec select
         [x _ 0] x
         [_ y _] y)

(primrec even
         [0] (S 0)
         [(S x)] (Z? (even x)))

(primrec ^{:doc "floor of x/2"}
         div2
         [0] 0
         [(S x)] (add (even (S x)) (div2 x)))

(primrec lg-rec
         [x 0] (pred x)
         [x (S y)] (select (lg-rec x y)
                           (sub x (S (S y)))
                           (grt (exp (S (S 0)) (sub x (S y))) x)))

(primrec ^{:doc "floor of base 2 logarithm"}
         lg [x] (lg-rec x x))

(primrec ^{:doc "trianlge numbers: n*(n-1)/2"}
         triangle
         [0] 0
         [(S x)] (add (triangle x) (S x)))

(primrec inv-triangle-rec
         [x 0] (select x
                       (pred x)
                       (grt (triangle x) x))
         [x (S y)] (select (inv-triangle-rec x y)
                           (sub x (S (S y)))
                           (grt (triangle (sub x (S y))) x)))

(primrec ^{:doc "inverse of the next smaller or equal triangle number"}
         inv-triangle [x] (inv-triangle-rec x x))

(primrec pair
         [x y] (add (triangle (add x y)) y))

(primrec left
         [x] (sub (inv-triangle x) (sub x (triangle (inv-triangle x)))))

(primrec right
         [x] (sub x (triangle (inv-triangle x))))
