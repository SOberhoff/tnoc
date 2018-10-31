(ns tnoc.ip-test
  (:require [tnoc.ip :refer :all]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [com.gfredericks.test.chuck.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as tcgen]))

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

(deftest phi-test
  (is (every? identity (map :verified? (interact phi [3 2 5])))))

(deftest phi-R-test
  (is (every? identity (map :verified? (interact phi-R [3 2 5])))))

(def polynomial-and-substitutions-gen
  (tcgen/let [polynomial (spec/gen :tnoc.ip/polynomial)
              substitutions (let [variables (->> (flatten polynomial)
                                                 (filter keyword?)
                                                 (distinct))]
                              (->> (count variables)
                                   (tcgen/vector tcgen/int)
                                   (tcgen/fmap #(zipmap variables %))))]
             [polynomial substitutions]))

(defspec simplify-test 5
         (for-all [[polynomial substitutions] polynomial-and-substitutions-gen]
                  (= (simplify (substitute polynomial substitutions))
                     (simplify (substitute (simplify polynomial) substitutions)))))

(defspec simplify-non-distributive-test 5
         (for-all [[polynomial substitutions] polynomial-and-substitutions-gen]
                  (= (simplify (substitute polynomial substitutions))
                     (simplify (substitute (simplify polynomial false) substitutions)))))

(defspec arithmetize-simplifying-test 5
         (for-all [formula (spec/gen :tnoc.ip/formula)]
                  (= (simplify (arithmetize formula))
                     (arithmetize-simplifying formula))))

(defspec ip-test 5
         (for-all [formula (spec/gen :tnoc.ip/formula)]
                  (->> (repeatedly #(rand-int 20))
                       (interact formula)
                       (map :verified?)
                       (every? identity))))