(ns tnoc.post)

(defn new-strings [string pair-map]
  (for [prefix (keys pair-map)
        :when (.startsWith string prefix)]
    (str (subs string (count prefix)) (pair-map prefix))))

(defn run [initial-string pair-map]
  (->> (list initial-string)
       (iterate (partial mapcat #(new-strings % pair-map)))
       (take-while not-empty)))

(def alphabet ["0" "1" "_" "|" "$"])

(def default-pairs (mapv #(vector % %) alphabet))

(defn move-right-pairs [current-state-name next-state-name current-symbol-name next-symbol-name]
  (for [symbol-2-name alphabet]
    [(str current-state-name current-symbol-name symbol-2-name)
     (if-not (= "|" symbol-2-name)
       (str next-symbol-name next-state-name symbol-2-name)
       (str next-symbol-name next-state-name "_|"))]))

(defn move-left-pairs [current-state-name]
  (concat
    (for [symbol-1-name alphabet
          symbol-2-name alphabet]
      [(str "'" current-state-name symbol-1-name symbol-2-name)
       (str symbol-1-name "'" current-state-name symbol-2-name)])
    (for [symbol-name alphabet]
      [(str "'" current-state-name symbol-name "$")
       (if-not (= "|" symbol-name)
         (str current-state-name symbol-name)
         (str "|" current-state-name "_"))])))

(defn transition-pairs [turing-machine]
  (apply concat
         (for [[current-state transition-fns] (turing-machine :STATES)
               [current-symbol [next-state next-symbol direction]] transition-fns
               :let [current-state-name (str "(" (name current-state) ")")
                     next-state-name (str "(" (name next-state) ")")
                     current-symbol-name (name current-symbol)
                     next-symbol-name (name next-symbol)]]
           (case direction
             :>> (move-right-pairs current-state-name next-state-name current-symbol-name next-symbol-name)
             :<< (conj (move-left-pairs next-state-name)
                       [(str current-state-name current-symbol-name)
                        (str "$" next-symbol-name "'" next-state-name)])
             :<> [[(str current-state-name current-symbol-name)
                   (str next-state-name next-symbol-name)]]))))

(defn compile-turing-machine [input turing-machine]
  [(str "(" (name (turing-machine :START)) ")" input "_|_")
   (-> (into {} default-pairs)
       (into (transition-pairs turing-machine)))])

(defn filter-turing-steps [strings]
  (filter #(.startsWith % "(") strings))
