(ns tnoc.post-test
  (:require [tnoc.post :refer :all]
            [tnoc.turing-test :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check.generators :as tcgen]
            [com.gfredericks.test.chuck.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]))

(deftest post-even-turing-accept-test
  (is (str/starts-with?
        (->> (compile-to-post-canonical-system even-turing even-turing-accepting-initial-configuration)
             (apply run-post-canonical-system)
             (last)
             (first))
        "(EVEN)")))

(deftest post-even-turing-reject-test
  (is (str/starts-with?
        (->> (compile-to-post-canonical-system even-turing even-turing-accepting-rejecting-configuration)
             (apply run-post-canonical-system)
             (last)
             (first))
        "(ODD)")))

(defspec compile-decompile-to-post-canonical-system-test
         (for-all [alphabet (tcgen/set tcgen/char-alpha {:min-elements 1})
                   [turing-machine configuration] (make-turing-machine-gen alphabet)]
                  (->> (compile-to-post-canonical-system turing-machine configuration)
                       (second)
                       (decompile-string)
                       (= configuration))))
