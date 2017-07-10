(ns tnoc.three-sat-diophantine
  (:require [tnoc.subset-diophantine :refer [prime? merge-meshes]]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as sgen]))

(defn preprocess [formula]
  (->> formula
       (distinct)
       (filter (fn [clause]
                 (every? not (for [x clause, y clause]
                               (or (and (seq? x) (= (second x) y))
                                   (and (seq? y) (= (second y) x)))))))))

(defn get-variables [formula]
  (->> formula
       (flatten)
       (distinct)
       (filter (comp not #{'!}))))

(defn compute-fi's [formula]
  (->> formula
       (get-variables)
       (map (fn [var]
              (let [fi+ (reduce +' (map-indexed #(if (some #{var} %2) (expt 8 (inc %1)) 0) formula))
                    fi- (reduce +' (map-indexed #(if (some #{(list '! var)} %2) (expt 8 (inc %1)) 0) formula))]
                {:var var :fi+ fi+ :fi- fi-})))))

(defn compute-n [formula] (+ (* 2 (count formula) (count (get-variables formula)))))

(defn compute-cj's [formula]
  (let [c1-to-2m (->> (range 1 (inc (* 2 (count formula))))
                      (map #(if (odd? %)
                              (*' -1/2 (expt 8 (/ (inc %) 2)))
                              (*' -1 (expt 8 (/ % 2))))))
        c2m+1-to-2m+l (->> (compute-fi's formula)
                           (map #(*' 1/2 (-' (:fi+ %) (:fi- %)))))]
    (concat [1] c1-to-2m c2m+1-to-2m+l)))

(defn compute-tau [formula]
  (let [tau-phi (reduce +' (map #(*' -1 (expt 8 %)) (range 1 (inc (count formula)))))
        cj-sum (reduce +' (compute-cj's formula))
        fi--sum (reduce +' (map :fi- (compute-fi's formula)))]
    (+' tau-phi cj-sum fi--sum)))

(def primes-greater-12 (filter prime? (iterate inc 13)))

(defn compute-thetas [formula]
  (let [cj's (compute-cj's formula)
        m (count formula)
        n (count cj's)
        pj's (take m primes-greater-12)]
    (->> (map vector cj's pj's)
         (map (fn [[cj pj]]
                (->> (range 1 pj)
                     (map (fn [remainder]
                            (reduce merge-meshes
                                    [[(expt 8 (inc m)) cj]
                                     [(reduce *' (map #(expt % (inc n)) (remove #{pj} pj's))) 0]
                                     [pj remainder]])))
                     (map second)
                     (apply min)))))))

(defn setup-equation [formula]
  (let [thetas (compute-thetas formula)
        m (count formula)
        n (count thetas)
        H (reduce +' thetas)
        K (reduce *' (map #(expt % (inc n)) (take m primes-greater-12)))
        tau (compute-tau formula)]
    `(~'fn [~'x ~'y] (~'= (~'+'
                            (~'*'
                              ~(*' (expt (inc K) 3) 2 (expt 8 (inc m)))
                              (~'-' ~(*' H H) (~'*' ~'x ~'x)))
                            (~'*' ~K (~'-' (~'*' ~'x ~'x) ~(*' tau tau)))
                            (~'*' ~(*' -2 (expt 8 (inc m)) K) ~'y))
                       0))))
