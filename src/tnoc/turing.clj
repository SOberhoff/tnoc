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
  {:W0 {\0 [\A :>> :W1]
        \1 [\B :>> :W2]
        \A [\A :>> :W0]
        \B [\B :>> :W0]
        \C [\C :>> :W0]
        \D [\D :>> :W0]
        \M [\M :>> :W0]
        \X [\X :>> :W0]}
   :W1 {\0 [\A :<< :W3]
        \1 [\B :<< :W4]}
   :W2 {\0 [\A :<< :W5]
        \1 [\B :<< :W6]}
   :W3 {\0 [\A :<< :RD]
        \1 [\A :<< :RD]
        \A [\A :<< :W3]
        \B [\B :<< :W3]
        \C [\C :<< :W3]
        \D [\D :<< :W3]
        \M [\M :<< :W3]
        \X [\X :<< :W3]}
   :W4 {\0 [\0 :>> :RD]
        \1 [\0 :>> :RD]
        \A [\A :<< :W4]
        \B [\B :<< :W4]
        \C [\C :<< :W4]
        \D [\D :<< :W4]
        \M [\M :<< :W4]
        \X [\X :<< :W4]}
   :W5 {\0 [\B :<< :RD]
        \1 [\B :<< :RD]
        \A [\A :<< :W5]
        \B [\B :<< :W5]
        \C [\C :<< :W5]
        \D [\D :<< :W5]
        \M [\M :<< :W5]
        \X [\X :<< :W5]}
   :W6 {\0 [\1 :>> :RD]
        \1 [\1 :>> :RD]
        \A [\A :<< :W6]
        \B [\B :<< :W6]
        \C [\C :<< :W6]
        \D [\D :<< :W6]
        \M [\M :<< :W6]
        \X [\X :<< :W6]}
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
   :FW  {\0 [\A :>> :A1]
        \1 [\B :<< :B1]
        \A [\A :>> :FW]
        \B [\B :>> :FW]
        \C [\C :<< :B3]
        \D [\D :>> :FW]
        \M [\M :>> :FW]
        \X [\M :>> :W0]}
   :A1 {\0 [\0 :>> :A1]
        \1 [\1 :>> :A1]
        \B [\B :>> :A1]
        \C [\C :>> :A1]
        \M [\B :>> :A1]
        \X [\M :<< :A2]}
   :A2 {\0 [\0 :<< :A2]
        \1 [\1 :<< :A2]
        \A [\A :>> :A3]
        \C [\C :<< :A2]}
   :A3 {\0 [\A :>> :A1]
        \B [\M :>> :A4]
        \C [\D :>> :A4]
        \M [\M :>> :W0]}
   :A4 {\0 [\A :>> :A4]
        \1 [\B :>> :A4]
        \B [\M :>> :A4]
        \C [\D :>> :A4]
        \M [\M :>> :W0]}
   :B1 {\0 [\0 :<< :B1]
        \A [\A :<< :B1]
        \B [\B :<< :B1]
        \D [\D :<< :B1]
        \M [\0 :>> :B2]}
   :B2 {\0 [\0 :>> :B2]
        \1 [\B :<< :B1]
        \A [\A :>> :B2]
        \B [\B :>> :B2]
        \C [\C :<< :B3]
        \D [\D :>> :B2]
        \X [\X :<< :B3]}
   :B3 {\0 [\X :<< :B3]
        \A [\0 :<< :B3]
        \B [\1 :<< :B3]
        \D [\C :<< :B3]
        \M [\M :>> :W0]}})

(def U-even-config
  {:STATE    :W0
   :TAPE     "1010M00C0X100C00X00C0X10111C11X"
   :POSITION 4})
