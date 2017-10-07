(ns tnoc.post)

(defn new-strings [string pair-map max-prefix-length]
  (->> (range 1 (inc max-prefix-length))
       (mapcat (fn [length] (->> (pair-map (subs string 0 length))
                                 (map #(str (subs string length) %)))))))

(defn run [pair-map initial-string]
  (let [max-prefix-length (apply max (map count (keys pair-map)))]
    (->> (list initial-string)
         (iterate (partial mapcat #(new-strings % pair-map max-prefix-length)))
         (take-while not-empty))))

(defn move-right-pairs [current-state-name next-state-name current-symbol next-symbol alphabet]
  (for [symbol-2-name alphabet]
    [(str current-state-name current-symbol symbol-2-name)
     #{(if-not (= \| symbol-2-name)
         (str next-symbol next-state-name symbol-2-name)
         (str next-symbol next-state-name "_|"))}]))

(defn move-left-pairs [current-state-name alphabet]
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

(defn transition-pairs [turing-machine alphabet]
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

(defn compile-turing-machine [turing-machine initial-configuration]
  (let [alphabet (into [\| \$] (get-alphabet turing-machine))]
    [(-> (reduce #(assoc %1 (str %2) #{(str %2)}) {} alphabet)
         (into (transition-pairs turing-machine alphabet)))
     (str \( (name (initial-configuration :STATE)) \)
          (subs (initial-configuration :TAPE) (initial-configuration :POSITION))
          \| (subs (initial-configuration :TAPE) 0 (initial-configuration :POSITION)))]))

(defn filter-turing-steps [strings] (filter #(.startsWith % "(") strings))
