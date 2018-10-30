(ns tnoc.turing
  (:require [clojure.spec.alpha :as spec]
            [com.rpl.specter :as spt]
            [clojure.string :as str]
            [tnoc.utils :refer :all]))

(load "turing_spec")

(defn next-configuration [turing-machine {state :state tape :tape position :position}]
  "Returns the next configuration of a Turing machine given a description of the Turing machine
  and its current configuration. Returns nil if no transition is possible."
  (if-let [[next-symbol direction next-state] ((turing-machine state) (.charAt tape position))]
    (let [modified-tape (str (subs tape 0 position) next-symbol (subs tape (inc position)))
          next-position (case direction
                          :>> (inc position)
                          :<< (if (zero? position) 0 (dec position))
                          :<> position)]
      {:state    next-state
       :tape     (cond
                   (and (zero? position) (= :<< direction)) (str \_ modified-tape)
                   (= next-position (count tape)) (str modified-tape \_)
                   :else modified-tape)
       :position next-position})))

(defn run-turing-machine [turing-machine configuration]
  "Returns the sequence of configurations gone through by a Turing machine during execution."
  (take-while some? (iterate (partial next-configuration turing-machine) configuration)))



(defn get-alphabet [turing-machine]
  "Returns the set of all characters that are either read or written by the given Turing machine."
  (-> #{}
      (into (mapcat keys (vals turing-machine)))
      (into (mapcat #(map first (vals %)) (vals turing-machine)))))

(defn pprint-configuration [{state :state tape :tape position :position}]
  "Converts a configuration into a more readable string."
  (str (name state) ": " (subs tape 0 position) "." (.charAt tape position) "." (subs tape (inc position))))



(defn- expand-multi-key [transition-fns]
  "Expands vectors used as keys in the definition of Turing machine transition functions into the
  corresponding character-key transition functions."
  (-> (fn [new-transition-fns [key val]]
        (if (vector? key)
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

(defmacro def-tm [name specification]
  "A Turing machine is defined as a map from states to transition functions. Transition functions
  in turn are a map that associates current symbols with transitions. Finally transitions are a
  3-tuple of the form [next-symbol direction next-state].
  Using this macro also allows a few shorthand notations. If several current symbols lead to the
  same transition, those current symbols can be grouped together into a vector that maps to the shared
  transition. Also if the current symbol or the current state aren't changed, they may be omitted
  from transitions. (Omitting the next symbol is particularly useful when grouping together current
  symbols.)"
  `(def ~name
     ~(->> specification
           (spt/transform [spt/MAP-VALS] expand-multi-key)
           (map infer-transitions)
           (into (sorted-map)))))



; A universal Turing machine
(def-tm
  U
  {:READ            {\o      [\O :<< :TO-STATE-O]
                     \i      [\I :<< :TO-STATE-I]
                     \u      [\U :<< :TO-STATE-U]
                     \_      [\U :<< :TO-STATE-U]
                     [\a \A] [:>> :INIT-SHIFT]}

   ; shift over the tape by one place to make space on the left
   :INIT-SHIFT      {\o [\u :>> :SHIFT-O]
                     \i [\u :>> :SHIFT-I]
                     \u [\u :>> :SHIFT-U]}
   :SHIFT-O         {\o [:>>]
                     \i [\o :>> :SHIFT-I]
                     \u [\o :>> :SHIFT-U]
                     \_ [\o :<< :TO-START]}
   :SHIFT-I         {\o [\i :>> :SHIFT-O]
                     \i [:>>]
                     \u [\i :>> :SHIFT-U]
                     \_ [\i :<< :TO-START]}
   :SHIFT-U         {\o [\u :>> :SHIFT-O]
                     \i [\u :>> :SHIFT-I]
                     \u [:>>]
                     \_ [\u :<< :TO-START]}
   :TO-START        {[\o \i \u] [:<<]
                     [\a \A]    [:>> :READ]}

   ; move to the description of the current state
   :TO-STATE-O      {[\o \i \u \l \r \s \t \a \b \c] [:<<]
                     \A                              [\a :<< :INIT-MARK]}
   :TO-STATE-I      {[\o \i \u \l \r \s \t \a \b \c] [:<<]
                     \A                              [\a :<< :TO-TRANSITION-I]}
   :TO-STATE-U      {[\o \i \u \l \r \s \t \a \b \c] [:<<]
                     \A                              [\a :<< :TO-TRANSITION-U]}

   ; move to the description of the appropriate transition
   :TO-TRANSITION-I {[\o \i \u \l \r \s \t \a] [:<<]
                     \b                        [:<< :INIT-MARK]}
   :TO-TRANSITION-U {[\o \i \u \l \r \s \t \a \b] [:<<]
                     \c                           [:<< :INIT-MARK]}

   ; mark which state to transition to
   :INIT-MARK       {\l [\L :<< :<<MARK]
                     \r [\R :>> :>>MARK]
                     \o [:<< :MOVE-O]
                     \i [:<< :MOVE-I]
                     \u [:<< :MOVE-U]}
   :<<MARK          {[\o \i \u \l \r \s \t \@ \b \c] [:<<]
                     \A                              [\@ :<<]
                     \a                              [\A :>> :>>REWIND]}
   :>>MARK          {[\o \i \u \l \r \s \t \@ \b \c \R] [:>>]
                     \A                                 [\@ :>>]
                     \a                                 [\A :<< :<<REWIND]}
   :>>REWIND        {[\o \i \u \l \r \s \t \b \c] [:>>]
                     \L                           [\l :<< :INIT-MARK]
                     \R                           [\r :<< :INIT-MARK]}
   :<<REWIND        {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \L                           [\l :<< :INIT-MARK]
                     \R                           [\r :<< :INIT-MARK]}

   ; determine in which direction to move next
   :MOVE-O          {\l [:>> :INIT-CLEAN-O-L]
                     \r [:>> :INIT-CLEAN-O-R]
                     \s [:>> :INIT-CLEAN-O-S]}
   :MOVE-I          {\l [:>> :INIT-CLEAN-I-L]
                     \r [:>> :INIT-CLEAN-I-R]
                     \s [:>> :INIT-CLEAN-I-S]}
   :MOVE-U          {\l [:>> :INIT-CLEAN-U-L]
                     \r [:>> :INIT-CLEAN-U-R]
                     \s [:>> :INIT-CLEAN-U-S]}

   ; figure out in which direction to clean up
   :INIT-CLEAN-O-L  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-O-L]
                     \R         [:>> :>>CLEAN-O-L]}
   :INIT-CLEAN-O-R  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-O-R]
                     \R         [:>> :>>CLEAN-O-R]}
   :INIT-CLEAN-O-S  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-O-S]
                     \R         [:>> :>>CLEAN-O-S]}
   :INIT-CLEAN-I-L  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-I-L]
                     \R         [:>> :>>CLEAN-I-L]}
   :INIT-CLEAN-I-R  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-I-R]
                     \R         [:>> :>>CLEAN-I-R]}
   :INIT-CLEAN-I-S  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-I-S]
                     \R         [:>> :>>CLEAN-I-S]}
   :INIT-CLEAN-U-L  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-U-L]
                     \R         [:>> :>>CLEAN-U-L]}
   :INIT-CLEAN-U-R  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-U-R]
                     \R         [:>> :>>CLEAN-U-R]}
   :INIT-CLEAN-U-S  {[\o \i \u] [:>>]
                     \L         [:<< :<<CLEAN-U-S]
                     \R         [:>> :>>CLEAN-U-S]}

   ; clean up to the left
   :<<CLEAN-O-L     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-O-L]}
   :<<CLEAN-O-R     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-O-R]}
   :<<CLEAN-O-S     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-O-S]}
   :<<CLEAN-I-L     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-I-L]}
   :<<CLEAN-I-R     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-I-R]}
   :<<CLEAN-I-S     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-I-S]}
   :<<CLEAN-U-L     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-U-L]}
   :<<CLEAN-U-R     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-U-R]}
   :<<CLEAN-U-S     {[\o \i \u \l \r \s \t \b \c] [:<<]
                     \@                           [\a :<<]
                     \A                           [:>> :TO-TAPE-U-S]}

   ; clean up to the right
   :>>CLEAN-O-L     {[\o \i \u \l \r \s \t \b \c] [:>>]
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-O-L]}
   :>>CLEAN-O-R     {[\o \i \u \l \r \s \t \b \c] [:>>]
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-O-R]}
   :>>CLEAN-O-S     {[\o \i \u \l \r \s \t \b \c] [:>>]
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-O-S]}
   :>>CLEAN-I-L     {[\o \i \u \l \r \s \t \b \c] :>>
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-I-L]}
   :>>CLEAN-I-R     {[\o \i \u \l \r \s \t \b \c] :>>
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-I-R]}
   :>>CLEAN-I-S     {[\o \i \u \l \r \s \t \b \c] [:>>]
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-I-S]}
   :>>CLEAN-U-L     {[\o \i \u \l \r \s \t \b \c] [:>>]
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-U-L]}
   :>>CLEAN-U-R     {[\o \i \u \l \r \s \t \b \c] [:>>]
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-U-R]}
   :>>CLEAN-U-S     {[\o \i \u \l \r \s \t \b \c] [:>>]
                     \@                           [\a :>>]
                     \A                           [:>> :TO-TAPE-U-S]}

   ; move back to the marked position on the tape
   :TO-TAPE-O-L     {[\o \i \u \l \r \s \t \a \b \c] [:>>]
                     [\O \I \U]                      [\o :>> :READ]}
   :TO-TAPE-O-R     {[\o \i \u \l \r \s \t \a \b \c] [:<<]
                     [\O \I \U]                      [\o :>> :READ]}
   :TO-TAPE-O-S     {[\o \i \u \l \r \s \t \a \b \c] [:<>]
                     [\O \I \U]                      [\o :>> :READ]}
   :TO-TAPE-I-L     {[\o \i \u \l \r \s \t \a \b \c] [:>>]
                     [\O \I \U]                      [\i :>> :READ]}
   :TO-TAPE-I-R     {[\o \i \u \l \r \s \t \a \b \c] [:<<]
                     [\O \I \U]                      [\i :>> :READ]}
   :TO-TAPE-I-S     {[\o \i \u \l \r \s \t \a \b \c] [:<>]
                     [\O \I \U]                      [\i :>> :READ]}
   :TO-TAPE-U-L     {[\o \i \u \l \r \s \t \a \b \c] [:>>]
                     [\O \I \U]                      [\u :>> :READ]}
   :TO-TAPE-U-R     {[\o \i \u \l \r \s \t \a \b \c] [:<<]
                     [\O \I \U]                      [\u :>> :READ]}
   :TO-TAPE-U-S     {[\o \i \u \l \r \s \t \a \b \c] [:<>]
                     [\O \I \U]                      [\u :>> :READ]}})



(defn- get-offset-indicator [current-index next-index]
  "Creates a string with the requisite number of l's or r's to move from the transition with
  current-index to the transition with next-index.
  Example: current-index = 5, next-index = 2 -> lll"
  (apply str
         (if (<= current-index next-index)
           (repeat (inc (- next-index current-index)) \r)
           (repeat (- current-index next-index) \l))))

(defn- serialize-transition [turing-machine current-state symbol]
  "Creates a string representing the transition from current-state after having read the given symbol.
  Example: lilll"
  (let [transition-fn (turing-machine current-state)
        [next-symbol direction next-state :as transition] (transition-fn symbol)]
    (if (nil? transition)
      "t"
      (str (case direction :<< \l :<> \s :>> \r)
           (case next-symbol \0 \o \1 \i \_ \u)
           (get-offset-indicator (index-of #{current-state} (keys turing-machine))
                                 (index-of #{next-state} (keys turing-machine)))))))

(defn- serialize-state [turing-machine state]
  "Creates a string representing the given state.
  Example: lilllcrorrrbta"
  (str (serialize-transition turing-machine state \0)
       \c
       (serialize-transition turing-machine state \1)
       \b
       (serialize-transition turing-machine state \_)
       \a))

(defn- mark [string index]
  (str (subs string 0 index)
       (case (.charAt string index)
         \o \O, \i \I, \u \U, \l \L, \r \R, \a \A)
       (subs string (inc index))))

(defn- serialize-turing-machine [turing-machine {state :state tape :tape position :position}]
  "Takes a Turing machine together with a configuration and produces a string that can be placed
  on the tape of the universal machine U."
  (let [compiled-states (str (->> (keys turing-machine)
                                  (map #(let [state-string (serialize-state turing-machine %)]
                                          (if (= state %)
                                            (mark state-string (dec (count state-string)))
                                            state-string)))
                                  (str/join)))
        compiled-tape (mark (apply str (map #(case % \0 \o \1 \i \_ \u) tape)) position)]
    (str compiled-states compiled-tape)))

(defn compile-to-U-configuration [turing-machine configuration]
  "Compiles a Turing machine together with a configuration into a configuration for the universal
  Turing machine U."
  (let [tape (serialize-turing-machine turing-machine configuration)]
    {:state    :WRITE?
     :tape     tape
     :position (max (.indexOf tape "O") (.indexOf tape "I") (.indexOf tape "U"))}))

(defn decompile-from-U-configuration [turing-machine {U-tape :tape}]
  "Reproduces the configuration of the given Turing machine from the given configuration of the
  universal Turing machine U."
  (let [state (nth (keys turing-machine) (index-of #{\A} (filter #{\a \A} U-tape)))
        tape (subs U-tape (inc (max (.lastIndexOf U-tape "a") (.lastIndexOf U-tape "A"))))]
    {:state    state
     :tape     (apply str (map #(case % \o \0 \i \1 \u \_) (str/lower-case tape)))
     :position (max (.lastIndexOf tape "O") (.lastIndexOf tape "I") (.lastIndexOf tape "U"))}))
