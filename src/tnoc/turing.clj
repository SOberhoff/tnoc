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
      (into (mapcat #(map second (vals %)) (vals turing-machine)))))
