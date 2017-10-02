(ns tnoc.turing-test
  (:require [tnoc.turing :refer :all]
            [clojure.test :refer :all]))

(def even-turing
  {:EVEN {\0 [\0 :>> :EVEN]
          \1 [\1 :>> :ODD]}
   :ODD  {\0 [\0 :>> :ODD]
          \1 [\1 :>> :EVEN]}})

(def even-turing-accepting-initial-configuration
  {:STATE    :EVEN
   :TAPE     "1010"
   :POSITION 0})

(def even-turing-accepting-rejecting-configuration
  {:STATE    :EVEN
   :TAPE     "1011"
   :POSITION 0})

(deftest even-turing-accept-test
  (is (= :EVEN
         (->> (run even-turing even-turing-accepting-initial-configuration)
              (last)
              (:STATE)))))

(deftest even-turing-reject-test
  (is (= :ODD
         (->> (run even-turing even-turing-accepting-rejecting-configuration)
              (last)
              (:STATE)))))
