(ns tnoc.turing
  (:require [clojure.spec.alpha :as spec]
            [com.rpl.specter :as spt]
            [clojure.string :as str]))

(load "turing_spec")

(defn next-configuration [turing-machine {state :STATE tape :TAPE position :POSITION}]
  "Returns the next configuration of a Turing machine given a description of the Turing machine
  and its current configuration. Returns nil if no transition is possible."
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

(defn run-turing-machine [turing-machine configuration]
  "Returns the sequence of configurations gone through by a Turing machine during execution."
  (take-while some? (iterate (partial next-configuration turing-machine) configuration)))

(defn get-alphabet [turing-machine]
  "Returns the set of all characters that are either read or written by the given Turing machine."
  (-> #{}
      (into (mapcat keys (vals turing-machine)))
      (into (mapcat #(map first (vals %)) (vals turing-machine)))))

(defn pprint-configuration [{state :STATE tape :TAPE position :POSITION}]
  "Converts a configuration into a more readable string."
  (str (name state) ": " (subs tape 0 position) "." (.charAt tape position) "." (subs tape (inc position))))


(defn- expand-multi-key [transition-fns]
  "Expands sets used as keys in the definition of Turing machine transition functions into the
  corresponding character-key transition functions."
  (-> (fn [new-transition-fns [key val]]
        (if (set? key)
          (reduce #(assoc %1 %2 val) new-transition-fns key)
          (assoc new-transition-fns key val)))
      (reduce {} transition-fns)))

(defn- infer-transitions [[current-state transition-fns]]
  "Adds omitted next-symbols and next-states to transitions."
  (->> transition-fns
       (map (fn [[current-symbol transition]]
              (let [{next-symbol :next-symbol direction :direction next-state :next-state}
                    (spec/conform ::transition' transition)]
                [current-symbol [(if next-symbol next-symbol current-symbol)
                                 direction
                                 (if next-state next-state current-state)]])))
       (into {})
       (conj [current-state])))

(defmacro def-turing-machine [name specification]
  "A Turing machine is defined as a map from states to transition functions. Transition functions
  in turn are a map that associates current symbols with transitions. Finally transitions are a
  3-tuple of the form [next-symbol direction next-state].
  Using this macro also allows a few shorthand notations. If several current symbols lead to the
  same transition, those current symbols can be grouped together into a set that maps to the shared
  transition. Also if the current symbol or the current state aren't changed, they may be omitted
  from transitions. (Omitting the next symbol is particularly useful when grouping together current
  symbols.)"
  `(def ~name
     ~(->> specification
           (spt/transform [spt/MAP-VALS] expand-multi-key)
           (map infer-transitions)
           (into {}))))

; A universal Turing machine
(def-turing-machine
  U
  {:WRITE?          {\0                   [\A :<< :DIRECTION?-0]
                     \1                   [\B :<< :DIRECTION?-1]
                     #{\A \B \C \D \M \X} [:<<]}
   :DIRECTION?-0    {\0 [\A :>> :GOTO-TAPE-0]
                     \1 [\B :>> :GOTO-TAPE-1]}
   :DIRECTION?-1    {\0 [\A :>> :GOTO-TAPE-2]
                     \1 [\B :>> :GOTO-TAPE-3]}
   :GOTO-TAPE-0     {\0                   [\0 :<< :READ]
                     \1                   [\0 :<< :READ]
                     #{\A \B \C \D \M \X} [:>>]}
   :GOTO-TAPE-1     {\0                   [\A :>> :READ]
                     \1                   [\A :>> :READ]
                     #{\A \B \C \D \M \X} [:>>]}
   :GOTO-TAPE-2     {\0                   [\1 :<< :READ]
                     \1                   [\1 :<< :READ]
                     #{\A \B \C \D \M \X} [:>>]}
   :GOTO-TAPE-3     {\0                   [\B :>> :READ]
                     \1                   [\B :>> :READ]
                     #{\A \B \C \D \M \X} [:>>]}
   :READ            {\0 [\0 :<< :IF-ZERO]
                     \1 [\1 :<< :GOTO-TRANSITION]
                     \A [\0 :<< :IF-ZERO]
                     \B [\1 :<< :GOTO-TRANSITION]}
   :IF-ZERO         {\0             [\A :<<]
                     \1             [\B :<<]
                     \C             [\D :<< :GOTO-TRANSITION]
                     #{\A \B \D \M} [:<<]}
   :GOTO-TRANSITION {\0             [\A :<< :<<MARK]
                     \1             [\B :>> :>>MARK]
                     #{\C \X}       [:>> :>>CLEANUP]
                     #{\A \B \D \M} [:<<]}
   :>>MARK          {\M             [\0 :<< :>>REWIND]
                     #{\0 \A \B \D} [:>>]}
   :>>REWIND        {\1             [\B :>> :>>MARK]
                     #{\C \X}       [:>> :>>CLEANUP]
                     #{\0 \A \B \D} [:<<]}
   :>>CLEANUP       {\0 [\X :>>]
                     \A [\0 :>>]
                     \B [\1 :>>]
                     \D [\C :>>]
                     \M [:<< :WRITE?]}
   :<<MARK          {#{\0 \1 \B \C} [:<<]
                     \M             [\B :<<]
                     \X             [\M :>> :<<REWIND]}
   :<<REWIND        {#{\0 \1 \B \C} [:>>]
                     \A             [:<< :<<MARK-AGAIN?]}
   :<<MARK-AGAIN?   {\0 [\A :<< :<<MARK]
                     \B [\M :<< :<<CLEANUP]
                     \C [\D :<< :<<CLEANUP]
                     \M [:<< :WRITE?]}
   :<<CLEANUP       {\0 [\A :<<]
                     \1 [\B :<<]
                     \B [\M :<<]
                     \C [\D :<<]
                     \M [:<< :WRITE?]}})

(defn- get-transition-index [turing-machine state symbol]
  "Finds the index of transition belonging to the given symbol and state in the given Turing machine."
  (->> (for [[current-state transition-fns] turing-machine
             [current-symbol _] transition-fns]
         [current-state current-symbol])
       (reduce #(if (= [state symbol] %2) (reduced %1) (inc %1)) 0)))

(defn- get-transition [turing-machine index]
  "Finds the transition at the given index in the given Turing machine."
  (nth (for [[_ transition-fns] turing-machine
             [_ transition] transition-fns]
         transition)
       index))

(defn- get-offset-indicator [current-index next-index]
  "Creates the string with the requisite number of 0's or 1's to move from the transition with
  `current-index` to the transition with `next-index`."
  (apply str
         (if (< current-index next-index)
           (repeat (- next-index current-index) \1)
           (repeat (- current-index next-index) \0))))

(defn- get-transition-strings [turing-machine]
  "Creates a sequence of strings representing all the transitions of the given Turing machine,
  excluding the leading X guard."
  (for [[current-state transition-fns] turing-machine
        [current-symbol [next-symbol direction next-state]] transition-fns
        :let [current-index (get-transition-index turing-machine current-state current-symbol)
              transition-index-if-0 (get-transition-index turing-machine next-state \0)
              transition-index-if-1 (get-transition-index turing-machine next-state \1)]]
    (str
      (get-offset-indicator current-index transition-index-if-0)
      \C
      (get-offset-indicator current-index transition-index-if-1)
      (if (= :<< direction) \0 \1)
      next-symbol
      \X)))

(defn- mark-string [string]
  "Converts unmarked symbols to their marked counterparts."
  (apply str (map #(case % \0 \A, \1 \B, \C \D, \X \M, %) string)))

(defn- unmark-string [string]
  "Converts marked symbols to their unmarked counterparts."
  (apply str (map #(case % \A \0, \B \1, \D \C, \X \M, %) string)))

(defn- mark-transition-strings [transition-strings turing-machine state first-symbol]
  "Marks the strings describing the transitions of the given Turing machine so as to reflect the
  first transition taking place."
  (let [first-transition-index (get-transition-index turing-machine state first-symbol)]
    (map-indexed #(cond (< first-transition-index %1) (mark-string %2)
                        (= first-transition-index %1) (str (subs %2 0 (dec (count %2))) \M)
                        :else %2)
                 transition-strings)))

(defn serialize-turing-machine [turing-machine {state :STATE tape :TAPE position :POSITION}]
  "Takes a Turing machine together with a configuration and produces a string that can be placed
  on the tape of the universal machine U."
  (str "X"
       (-> (get-transition-strings turing-machine)
           (mark-transition-strings turing-machine state (.charAt tape position))
           (str/join))
       (str (mark-string (subs tape 0 position)) (subs tape position))))

(defn convert-to-U-configuration [turing-machine configuration]
  "Converts a Turing machine together with a configuration into a configuration for the universal
  Turing machine U."
  (let [tape (serialize-turing-machine turing-machine configuration)]
    {:STATE    :WRITE?
     :TAPE     tape
     :POSITION (.indexOf tape "M")}))

(defn deconvert-from-U-configuration [turing-machine {U-tape :TAPE}]
  "Reproduces the configuration of the given Turing machine from the given configuration of the
  universal Turing machine U."
  (let [current-transition-index (->> (re-seq #"[01ABCD]+[MX]" U-tape)
                                      (reduce #(if (.endsWith %2 "M") (reduced %1) (inc %1)) 0))
        [_ _ next-state] (get-transition turing-machine current-transition-index)
        tape (subs U-tape (inc (.lastIndexOf U-tape "M")))]
    {:STATE    next-state
     :TAPE     (unmark-string tape)
     :POSITION (inc (max (.lastIndexOf tape "A") (.lastIndexOf tape "B")))}))
