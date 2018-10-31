(ns tnoc.turing-test
  (:require [tnoc.turing :refer :all]
            [clojure.test :refer :all]
            [clojure.walk :as walk]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck.properties :refer [for-all]]))

(def-tm
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

(defn make-turing-machine-gen [alphabet]
  (tcgen/let [states (tcgen/set (tcgen/fmap (comp keyword str) tcgen/char-alpha-numeric)
                                {:min-elements 1 :max-elements 10})
              transition-fns (let [state-gen (tcgen/elements states)
                                   transition-fn-gen (tcgen/map (tcgen/elements alphabet)
                                                                (tcgen/tuple (tcgen/elements alphabet)
                                                                             (tcgen/elements #{:<< :<> :>>})
                                                                             state-gen))]
                               (tcgen/vector transition-fn-gen (count states)))]
             (tcgen/let [initial-state (tcgen/elements states)
                         tape (tcgen/fmap #(apply str %) (tcgen/vector (tcgen/elements alphabet) 1 50))
                         position (tcgen/choose 0 (dec (count tape)))]
                        [(zipmap states transition-fns) {:state initial-state :tape tape :position position}])))

(defspec compile-to-U-decompile-test
         (for-all [[turing-machine configuration] (make-turing-machine-gen #{\0 \1 \_})]
                  (->> (compile-to-U-configuration turing-machine configuration)
                       (decompile-from-U-configuration turing-machine)
                       (= configuration))))

(defn- run-for-n-steps-on-U [turing-machine configuration n]
  (->> (compile-to-U-configuration turing-machine configuration)
       (run-turing-machine U)
       (filter #(= :READ (% :state)))
       (#(nth % n nil))
       (#(if % (decompile-from-U-configuration turing-machine %)))))

(defspec run-on-U-test 5
         (for-all [[turing-machine configuration] (make-turing-machine-gen #{\0 \1 \_})]
                  (= (nth (run-turing-machine turing-machine configuration) 10 nil)
                     (run-for-n-steps-on-U turing-machine configuration 10))))