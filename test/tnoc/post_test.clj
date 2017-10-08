(ns tnoc.post-test
  (:require [tnoc.post :refer :all]
            [tnoc.turing-test :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as string]))

(deftest post-even-turing-accept-test
  (is (string/starts-with?
        (->> (compile-to-post-canonical-system even-turing even-turing-accepting-initial-configuration)
             (apply run-post-canonical-system)
             (last)
             (first))
        "(EVEN)")))

(deftest post-even-turing-reject-test
  (is (string/starts-with?
        (->> (compile-to-post-canonical-system even-turing even-turing-accepting-rejecting-configuration)
             (apply run-post-canonical-system)
             (last)
             (first))
        "(ODD)")))
