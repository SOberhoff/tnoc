(ns tnoc.turing-test
  (:require [tnoc.turing :refer :all]
            [clojure.test :refer :all]
            [clojure.walk :as walk]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as tcprop]))

(def-turing-machine
  even-turing
  {:EVEN {\0 [:>>]
          \1 [:>> :ODD]}
   :ODD  {\0 [:>>]
          \1 [:>> :EVEN]}})

(def even-turing-accepting-initial-configuration
  {:state    :EVEN
   :tape     "1010"
   :position 0})

(def even-turing-accepting-rejecting-configuration
  {:state    :EVEN
   :tape     "1011"
   :position 0})

(deftest even-turing-accept-test
  (is (= :EVEN
         (->> (run-turing-machine even-turing even-turing-accepting-initial-configuration)
              (last)
              (:state)))))

(deftest even-turing-reject-test
  (is (= :ODD
         (->> (run-turing-machine even-turing even-turing-accepting-rejecting-configuration)
              (last)
              (:state)))))

(def turing-machine-gen
  (tcgen/let [alphabet (tcgen/set tcgen/char-alphanumeric {:min-elements 2})
              states (tcgen/set tcgen/keyword {:min-elements 1})]
             (let [symbol-gen (tcgen/elements alphabet)
                   state-gen (tcgen/elements states)
                   transition-gen (tcgen/tuple symbol-gen (tcgen/elements #{:<< :<> :>>}) state-gen)
                   transition-fns-gen (tcgen/map symbol-gen transition-gen {:min-elements 1})]
               (tcgen/let [turing-machine (tcgen/map state-gen transition-fns-gen {:min-elements 1})
                           initial-state state-gen
                           tape (tcgen/fmap #(apply str %) (tcgen/vector symbol-gen 1 50))
                           position (tcgen/choose 0 (dec (count tape)))]
                          [turing-machine {:state initial-state :tape tape :position position}]))))

;(defspec compile-to-U-decompile-test
;         (tcprop/for-all [[turing-machine configuration] turing-machine-gen]
;                         (->> (compile-to-U-configuration turing-machine configuration)
;                              (decompile-from-U-configuration turing-machine)
;                              (= configuration))))
