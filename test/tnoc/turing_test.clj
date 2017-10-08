(ns tnoc.turing-test
  (:require [tnoc.turing :refer :all]
            [clojure.test :refer :all]
            [clojure.walk :as walk]))

(def-turing-machine
  even-turing
  {:EVEN {\0 [:>>]
          \1 [:>> :ODD]}
   :ODD  {\0 [:>>]
          \1 [:>> :EVEN]}})

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
         (->> (run-turing-machine even-turing even-turing-accepting-initial-configuration)
              (last)
              (:STATE)))))

(deftest even-turing-reject-test
  (is (= :ODD
         (->> (run-turing-machine even-turing even-turing-accepting-rejecting-configuration)
              (last)
              (:STATE)))))

(def U-explicit
  {:WRITE?          {\0 [\A :>> :DIRECTION?-0]
                     \1 [\B :>> :DIRECTION?-1]
                     \A [\A :>> :WRITE?]
                     \B [\B :>> :WRITE?]
                     \C [\C :>> :WRITE?]
                     \D [\D :>> :WRITE?]
                     \M [\M :>> :WRITE?]
                     \X [\X :>> :WRITE?]}
   :DIRECTION?-0    {\0 [\A :<< :GOTO-TAPE-0]
                     \1 [\B :<< :GOTO-TAPE-1]}
   :DIRECTION?-1    {\0 [\A :<< :GOTO-TAPE-2]
                     \1 [\B :<< :GOTO-TAPE-3]}
   :GOTO-TAPE-0     {\0 [\A :<< :READ]
                     \1 [\A :<< :READ]
                     \A [\A :<< :GOTO-TAPE-0]
                     \B [\B :<< :GOTO-TAPE-0]
                     \C [\C :<< :GOTO-TAPE-0]
                     \D [\D :<< :GOTO-TAPE-0]
                     \M [\M :<< :GOTO-TAPE-0]
                     \X [\X :<< :GOTO-TAPE-0]}
   :GOTO-TAPE-1     {\0 [\0 :>> :READ]
                     \1 [\0 :>> :READ]
                     \A [\A :<< :GOTO-TAPE-1]
                     \B [\B :<< :GOTO-TAPE-1]
                     \C [\C :<< :GOTO-TAPE-1]
                     \D [\D :<< :GOTO-TAPE-1]
                     \M [\M :<< :GOTO-TAPE-1]
                     \X [\X :<< :GOTO-TAPE-1]}
   :GOTO-TAPE-2     {\0 [\B :<< :READ]
                     \1 [\B :<< :READ]
                     \A [\A :<< :GOTO-TAPE-2]
                     \B [\B :<< :GOTO-TAPE-2]
                     \C [\C :<< :GOTO-TAPE-2]
                     \D [\D :<< :GOTO-TAPE-2]
                     \M [\M :<< :GOTO-TAPE-2]
                     \X [\X :<< :GOTO-TAPE-2]}
   :GOTO-TAPE-3     {\0 [\1 :>> :READ]
                     \1 [\1 :>> :READ]
                     \A [\A :<< :GOTO-TAPE-3]
                     \B [\B :<< :GOTO-TAPE-3]
                     \C [\C :<< :GOTO-TAPE-3]
                     \D [\D :<< :GOTO-TAPE-3]
                     \M [\M :<< :GOTO-TAPE-3]
                     \X [\X :<< :GOTO-TAPE-3]}
   :READ            {\0 [\0 :>> :GOTO-TRANSITION]
                     \1 [\1 :>> :IF-ONE]
                     \A [\0 :>> :GOTO-TRANSITION]
                     \B [\1 :>> :IF-ONE]}
   :IF-ONE          {\0 [\A :>> :IF-ONE]
                     \1 [\B :>> :IF-ONE]
                     \A [\A :>> :IF-ONE]
                     \B [\B :>> :IF-ONE]
                     \C [\D :>> :GOTO-TRANSITION]
                     \D [\D :>> :IF-ONE]
                     \M [\M :>> :IF-ONE]}
   :GOTO-TRANSITION {\0 [\A :<< :<<MARK]
                     \1 [\B :>> :>>MARK]
                     \A [\A :>> :GOTO-TRANSITION]
                     \B [\B :>> :GOTO-TRANSITION]
                     \C [\C :<< :<<CLEANUP]
                     \D [\D :>> :GOTO-TRANSITION]
                     \M [\M :>> :GOTO-TRANSITION]
                     \X [\X :<< :<<CLEANUP]}
   :<<MARK          {\1 [\1 :<< :<<MARK]
                     \A [\A :<< :<<MARK]
                     \B [\B :<< :<<MARK]
                     \D [\D :<< :<<MARK]
                     \M [\1 :>> :<<REWIND]}
   :<<REWIND        {\0 [\A :<< :<<MARK]
                     \1 [\1 :>> :<<REWIND]
                     \A [\A :>> :<<REWIND]
                     \B [\B :>> :<<REWIND]
                     \C [\C :<< :<<CLEANUP]
                     \D [\D :>> :<<REWIND]
                     \X [\X :<< :<<CLEANUP]}
   :<<CLEANUP       {\1 [\X :<< :<<CLEANUP]
                     \A [\0 :<< :<<CLEANUP]
                     \B [\1 :<< :<<CLEANUP]
                     \D [\C :<< :<<CLEANUP]
                     \M [\M :>> :WRITE?]}
   :>>MARK          {\0 [\0 :>> :>>MARK]
                     \1 [\1 :>> :>>MARK]
                     \A [\A :>> :>>MARK]
                     \C [\C :>> :>>MARK]
                     \M [\A :>> :>>MARK]
                     \X [\M :<< :>>REWIND]}
   :>>REWIND        {\0 [\0 :<< :>>REWIND]
                     \1 [\1 :<< :>>REWIND]
                     \A [\A :<< :>>REWIND]
                     \B [\B :>> :>>MARK-AGAIN?]
                     \C [\C :<< :>>REWIND]}
   :>>MARK-AGAIN?   {\1 [\B :>> :>>MARK]
                     \A [\M :>> :>>CLEANUP]
                     \C [\D :>> :>>CLEANUP]
                     \M [\M :>> :WRITE?]}
   :>>CLEANUP       {\0 [\A :>> :>>CLEANUP]
                     \1 [\B :>> :>>CLEANUP]
                     \A [\M :>> :>>CLEANUP]
                     \C [\D :>> :>>CLEANUP]
                     \M [\M :>> :WRITE?]}})
