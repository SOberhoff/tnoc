(ns tnoc.post-test
  (:require [tnoc.post :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as string]))

(def even-turing {:STATES {:EVEN {:0 [:EVEN :0 :>>]
                                  :1 [:ODD :1 :>>]
                                  :_ [:ACCEPT :_ :<>]}
                           :ODD  {:0 [:ODD :0 :>>]
                                  :1 [:EVEN :1 :>>]
                                  :_ [:REJECT :_ :<>]}}
                  :START  :EVEN})

(deftest even-turing-accept-test
  (is (string/starts-with? (first (last (apply run (compile-turing-machine "1010" even-turing))))
                           "(ACCEPT)")))

(deftest even-turing-reject-test
  (is (string/starts-with? (first (last (apply run (compile-turing-machine "1011" even-turing))))
                           "(REJECT)")))
