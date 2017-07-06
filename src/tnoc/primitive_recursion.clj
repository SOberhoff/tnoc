(ns tnoc.primitive-recursion
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [com.rpl.specter :as spt]))

(load "primitive_recursion_spec")

(defn S-to-inc [body] (walk/postwalk-replace {'S 'inc} body))

(defn delay-force [body]
  (if-not (seq? body)
    `(force ~body)
    (->> body
         (walk/postwalk
           #(cond
              (and (seq? %) (#{'dec 'inc} (first %))) `(~(first %) (force ~(second %)))
              (seq? %) `(delay ~%)
              :else %))
         (list `force))))

(defn modify-recur [recur-params recur-body]
  (if (#{'_} (last recur-params))
    [recur-params recur-body]
    (let [modified-recur-params (spt/transform [spt/LAST] second recur-params)
          recur-param (second (last recur-params))
          modified-recur-body (walk/postwalk-replace {recur-param (list 'dec recur-param)} recur-body)]
      [modified-recur-params modified-recur-body])))

(defmacro primrec
  ([name params body]
   `(defn ~name ~params ~(delay-force (S-to-inc body))))
  ([name base-params base-body recur-params recur-body]
   (let [gen-params (into [] (repeatedly (count base-params) gensym))
         [modified-recur-params modified-recur-body] (modify-recur recur-params recur-body)]
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
         [x 0] (pred x)
         [x (S y)] (select (lg-rec x y)
                           (sub x (S (S y)))
                           (grt (exp (S (S 0)) (sub x (S y))) x)))

(primrec lg [x] (lg-rec x x))

(primrec triangle
         [0] 0
         [(S x)] (add (triangle x) (S x)))

(primrec inv-triangle-rec
         [x 0] (select x
                       (pred x)
                       (grt (triangle x) x))
         [x (S y)] (select (inv-triangle-rec x y)
                           (sub x (S (S y)))
                           (grt (triangle (sub x (S y))) x)))

(primrec inv-triangle [x] (inv-triangle-rec x x))

(primrec pair
         [x y] (add (triangle (add x y)) y))

(primrec left
         [x] (sub (inv-triangle x) (sub x (triangle (inv-triangle x)))))

(primrec right
         [x] (sub x (triangle (inv-triangle x))))
