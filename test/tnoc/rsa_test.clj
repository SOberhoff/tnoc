(ns tnoc.rsa-test
  (:require [tnoc.rsa :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.properties :as tcprop]))


(defspec string-to-bigint-to-string-test
         (tcprop/for-all [s tcgen/string-alphanumeric]
                         (= s (bigints-to-string (string-to-bigints s 8)))))

(defspec rsa-test
         (tcprop/for-all [s tcgen/string-alphanumeric]
                         (= s (decrypt (encrypt-with-new-keys s 64 17)))))
