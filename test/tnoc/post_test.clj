(ns tnoc.post-test
  (:require [tnoc.post :refer :all]
            [tnoc.turing-test :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as string]))

(deftest post-even-turing-accept-test
  (is (string/starts-with?
        (->> (compile-turing-machine even-turing-accepting-initial-configuration even-turing)
             (apply run)
             (last)
             (first))
        "(EVEN)")))

(deftest post-even-turing-reject-test
  (is (string/starts-with?
        (->> (compile-turing-machine even-turing-accepting-rejecting-configuration even-turing)
             (apply run)
             (last)
             (first))
        "(ODD)")))
