(ns tnoc.three-sat-integer-partition-test
  (:require [tnoc.three-sat-integer-partition :refer :all]
            [rolling-stones.core :as stones :refer [!]]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.alpha :as spec]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.properties :as tcprop]))

(defn integer-partition? [weights target]
  (->> weights
       (reduce (fn [sums weight] (mapcat #(vector (-' % weight) (+' % weight)) sums)) [0])
       (some #{target})))

(defn sat-equal-integer-partition? [formula]
  (let [satisfiable (boolean (stones/solve-symbolic-cnf formula))
        partitionable (boolean (integer-partition? (compute-cj's formula) (compute-tau formula)))]
    (= satisfiable partitionable)))

(deftest unsatisfiable-test
  (let [unsatisfiable [[(! :a) :b (! :c)]
                       [(! :a) (! :b) :c]
                       [(! :a) :b :c]
                       [:a (! :b) (! :c)]
                       [:a :b (! :c)]
                       [:a (! :b) :c]
                       [:a :b :c]
                       [(! :a) (! :b) (! :c)]]]
    (is (sat-equal-integer-partition? unsatisfiable))))

(defspec sat-equal-integer-partition-test
         (tcprop/for-all [formula (spec/gen :tnoc.three-sat-integer-partition/formula)]
                         (sat-equal-integer-partition? formula)))

(sat-equal-integer-partition-test)
