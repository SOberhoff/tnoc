(ns tnoc.post)

(defn new-strings [string pair-map]
  (for [prefix (keys pair-map)
        :when (.startsWith string prefix)]
    (str (subs string (count prefix)) (pair-map prefix))))

(defn run [pair-map initial-string]
  (->> (list initial-string)
       (iterate (partial mapcat #(new-strings % pair-map)))
       (take-while not-empty)))

(def alphabet [\0 \1 \_ \| \$])

(def default-pairs (map #(vector (str %) (str %)) alphabet))

(defn move-right-pairs [current-state-name next-state-name current-symbol next-symbol]
  (for [symbol-2-name alphabet]
    [(str current-state-name current-symbol symbol-2-name)
     (if-not (= \| symbol-2-name)
       (str next-symbol next-state-name symbol-2-name)
       (str next-symbol next-state-name "_|"))]))

(defn move-left-pairs [current-state-name]
  (concat
    (for [symbol-1-name alphabet
          symbol-2-name alphabet]
      [(str \' current-state-name symbol-1-name symbol-2-name)
       (str symbol-1-name \' current-state-name symbol-2-name)])
    (for [symbol-name alphabet]
      [(str \' current-state-name symbol-name \$)
       (if-not (= \| symbol-name)
         (str current-state-name symbol-name)
         (str \| current-state-name \_))])))

(defn transition-pairs [turing-machine]
  (apply concat
         (for [[current-state transition-fns] turing-machine
               [current-symbol [next-symbol direction next-state]] transition-fns
               :let [current-state-name (str \( (name current-state) \))
                     next-state-name (str \( (name next-state) \))]]
           (case direction
             :>> (move-right-pairs current-state-name next-state-name current-symbol next-symbol)
             :<< (conj (move-left-pairs next-state-name)
                       [(str current-state-name current-symbol)
                        (str \$ next-symbol \' next-state-name)])
             :<> [[(str current-state-name current-symbol)
                   (str next-state-name next-symbol)]]))))

(defn compile-turing-machine [initial-configuration turing-machine]
  [(-> (into {} default-pairs)
       (into (transition-pairs turing-machine)))
   (str \( (name (initial-configuration :STATE)) \)
        (subs (initial-configuration :TAPE) (initial-configuration :POSITION))
        \| (subs (initial-configuration :TAPE) 0 (initial-configuration :POSITION)))])

(defn filter-turing-steps [strings]
  (filter #(.startsWith % "(") strings))
