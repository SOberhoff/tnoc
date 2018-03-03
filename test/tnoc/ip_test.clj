(ns tnoc.ip-test
  (:require [tnoc.ip :refer :all]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.test.check.properties :as tcprop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def phi '(A :x (E :y (A :z (and (or :x :y :z) (or (not :x) (not :y) (not :z)))))))

(def phi-R
  '(A :x
      (R :x
         (E :y
            (R :x
               (R :y
                  (A :z
                     (R :x
                        (R :y
                           (R :z
                              (and (or :x :y :z) (or (not :x) (not :y) (not :z)))))))))))))

(defspec ip-test 5
         (tcprop/for-all [formula (spec/gen :tnoc.ip/formula)]
                         (->> (repeatedly #(rand-int 20))
                              (interact formula)
                              (map #(nth % 3))
                              (every? identity))))
