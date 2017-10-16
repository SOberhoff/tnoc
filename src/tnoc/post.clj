(ns tnoc.post
  (:require [tnoc.turing :refer [get-alphabet]]
            [clojure.string :as str]))

(defn- new-strings [post-cs string max-prefix-length]
  "Produces a sequence of all the strings derivable when applying the given Post canonical system
  to the given string. Also requires the maximal possible prefix length for performance reasons."
  (->> (range 1 (inc max-prefix-length))
       (mapcat (fn [length] (->> (post-cs (subs string 0 length))
                                 (map #(str (subs string length) %)))))))

(defn run-post-canonical-system [post-cs initial-string]
  "Produces a sequence of sequences describing all possible producible strings using the given
  Post canonical system and the given initial string."
  (let [max-prefix-length (apply max (map count (keys post-cs)))]
    (->> (list initial-string)
         (iterate (partial mapcat #(new-strings post-cs % max-prefix-length)))
         (take-while not-empty))))



(defn- move-right-pairs [current-state-name next-state-name current-symbol next-symbol alphabet]
  (for [symbol-2-name alphabet]
    [(str current-state-name current-symbol symbol-2-name)
     #{(if-not (= \| symbol-2-name)
         (str next-symbol next-state-name symbol-2-name)
         (str next-symbol next-state-name "_|"))}]))

(defn- move-left-pairs [current-state-name alphabet]
  (concat
    (for [symbol-1-name alphabet
          symbol-2-name alphabet]
      [(str \' current-state-name symbol-1-name symbol-2-name)
       #{(str symbol-1-name \' current-state-name symbol-2-name)}])
    (for [symbol-name alphabet]
      [(str \' current-state-name symbol-name \$)
       #{(if-not (= \| symbol-name)
           (str current-state-name symbol-name)
           (str \| current-state-name \_))}])))

(defn- transition-pairs [turing-machine alphabet]
  (apply concat
         (for [[current-state transition-fn] turing-machine
               [current-symbol [next-symbol direction next-state]] transition-fn
               :let [current-state-name (str \( (name current-state) \))
                     next-state-name (str \( (name next-state) \))]]
           (case direction
             :>> (move-right-pairs current-state-name next-state-name current-symbol next-symbol alphabet)
             :<< (conj (move-left-pairs next-state-name alphabet)
                       [(str current-state-name current-symbol)
                        #{(str \$ next-symbol \' next-state-name)}])
             :<> [[(str current-state-name current-symbol)
                   #{(str next-state-name next-symbol)}]]))))

(defn compile-to-post-canonical-system [turing-machine configuration]
  "Compiles a Turing machine together with its configuration to a Post canonical system and a
  corresponding initial string."
  (let [alphabet (into [\| \$] (get-alphabet turing-machine))]
    [(-> (reduce #(assoc %1 (str %2) #{(str %2)}) {} alphabet)
         (into (transition-pairs turing-machine alphabet)))
     (str \( (name (configuration :state)) \)
          (subs (configuration :tape) (configuration :position))
          \| (subs (configuration :tape) 0 (configuration :position)))]))

(defn filter-turing-steps [strings] (filter #(str/starts-with? % "(") strings))
