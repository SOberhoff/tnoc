(ns tnoc.turing
  (:require [clojure.spec.alpha :as spec]))

(defn next-configuration [turing-machine {state :STATE tape :TAPE position :POSITION}]
  (if-let [[next-symbol direction next-state] ((turing-machine state) (.charAt tape position))]
    (let [modified-tape (str (subs tape 0 position) next-symbol (subs tape (inc position)))
          next-position (case direction
                          :>> (inc position)
                          :<< (if (zero? position) 0 (dec position))
                          :<> position)]
      {:STATE    next-state
       :TAPE     (cond
                   (and (zero? position) (= :<< direction)) (str \_ modified-tape)
                   (= next-position (count tape)) (str modified-tape \_)
                   :else modified-tape)
       :POSITION next-position})))

(defn run [turing-machine configuration]
  (take-while some? (iterate (partial next-configuration turing-machine) configuration)))

(defn get-alphabet [turing-machine]
  (-> #{}
      (into (mapcat keys (vals turing-machine)))
      (into (mapcat #(map first (vals %)) (vals turing-machine)))))

(defn pprint-configuration [{state :STATE tape :TAPE position :POSITION}]
  (str (name state) ": " (subs tape 0 position) "." (.charAt tape position) "." (subs tape (inc position))))

(def U
  {:WR {\0 [\A :>> :D0]
        \1 [\B :>> :D1]
        \A [\A :>> :WR]
        \B [\B :>> :WR]
        \C [\C :>> :WR]
        \D [\D :>> :WR]
        \M [\M :>> :WR]
        \X [\X :>> :WR]}
   :D0 {\0 [\A :<< :R0]
        \1 [\B :<< :R1]}
   :D1 {\0 [\A :<< :R2]
        \1 [\B :<< :R3]}
   :R0 {\0 [\A :<< :RD]
        \1 [\A :<< :RD]
        \A [\A :<< :R0]
        \B [\B :<< :R0]
        \C [\C :<< :R0]
        \D [\D :<< :R0]
        \M [\M :<< :R0]
        \X [\X :<< :R0]}
   :R1 {\0 [\0 :>> :RD]
        \1 [\0 :>> :RD]
        \A [\A :<< :R1]
        \B [\B :<< :R1]
        \C [\C :<< :R1]
        \D [\D :<< :R1]
        \M [\M :<< :R1]
        \X [\X :<< :R1]}
   :R2 {\0 [\B :<< :RD]
        \1 [\B :<< :RD]
        \A [\A :<< :R2]
        \B [\B :<< :R2]
        \C [\C :<< :R2]
        \D [\D :<< :R2]
        \M [\M :<< :R2]
        \X [\X :<< :R2]}
   :R3 {\0 [\1 :>> :RD]
        \1 [\1 :>> :RD]
        \A [\A :<< :R3]
        \B [\B :<< :R3]
        \C [\C :<< :R3]
        \D [\D :<< :R3]
        \M [\M :<< :R3]
        \X [\X :<< :R3]}
   :RD {\0 [\0 :>> :FW]
        \1 [\1 :>> :I1]
        \A [\0 :>> :FW]
        \B [\1 :>> :I1]}
   :I1 {\0 [\A :>> :I1]
        \1 [\B :>> :I1]
        \A [\A :>> :I1]
        \B [\B :>> :I1]
        \C [\D :>> :FW]
        \D [\D :>> :I1]
        \M [\M :>> :I1]}
   :FW {\0 [\A :<< :<0]
        \1 [\B :>> :>0]
        \A [\A :>> :FW]
        \B [\B :>> :FW]
        \C [\C :<< :<2]
        \D [\D :>> :FW]
        \M [\M :>> :FW]
        \X [\X :<< :<2]}
   :<0 {\1 [\1 :<< :<0]
        \A [\A :<< :<0]
        \B [\B :<< :<0]
        \D [\D :<< :<0]
        \M [\1 :>> :<1]}
   :<1 {\0 [\A :<< :<0]
        \1 [\1 :>> :<1]
        \A [\A :>> :<1]
        \B [\B :>> :<1]
        \C [\C :<< :<2]
        \D [\D :>> :<1]
        \X [\X :<< :<2]}
   :<2 {\1 [\X :<< :<2]
        \A [\0 :<< :<2]
        \B [\1 :<< :<2]
        \D [\C :<< :<2]
        \M [\M :>> :WR]}
   :>0 {\0 [\0 :>> :>0]
        \1 [\1 :>> :>0]
        \A [\A :>> :>0]
        \C [\C :>> :>0]
        \M [\A :>> :>0]
        \X [\M :<< :>1]}
   :>1 {\0 [\0 :<< :>1]
        \1 [\1 :<< :>1]
        \A [\A :<< :>1]
        \B [\B :>> :>2]
        \C [\C :<< :>1]}
   :>2 {\1 [\B :>> :>0]
        \A [\M :>> :>3]
        \C [\D :>> :>3]
        \M [\M :>> :WR]}
   :>3 {\0 [\A :>> :>3]
        \1 [\B :>> :>3]
        \A [\M :>> :>3]
        \C [\D :>> :>3]
        \M [\M :>> :WR]}})

(def U-even-config
  {:STATE    :WR
   :TAPE     "1110M00C1X101C11X00C1X10000C00X"
   :POSITION 4})
