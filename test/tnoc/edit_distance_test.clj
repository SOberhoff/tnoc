(ns tnoc.edit-distance-test
  (:require [clojure.test :refer :all]
            [tnoc.edit-distance :refer :all]
            [clojure.spec.test.alpha :as stest]))

(deftest pastrycook-astronomer-test
  (is (= [6 "pastrycook_" "_astronomer"] (edit-distance "pastrycook" "astronomer"))))

(deftest edit-distance-gen-test
  (is (->> (stest/check 'tnoc.edit-distance/edit-distance {:clojure.spec.test.check/opts {:max-size 10}})
           (first)
           :clojure.spec.test.check/ret
           :pass?)))