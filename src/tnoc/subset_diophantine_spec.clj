(in-ns 'tnoc.subset-diophantine)

(spec/def ::nat+ (spec/and integer? (partial < 0)))

(spec/def ::nat+-vec (spec/coll-of ::nat+ :kind vector?))

(spec/def ::subset-sum-args
  (spec/with-gen (spec/cat :set ::nat+-vec, :target ::nat+)
                 #(sgen/fmap (fn [set] (vector set (rand-int (reduce + set)))) (spec/gen ::nat+-vec))))

(spec/fdef euclid
           :args (spec/cat :a ::nat+ :b ::nat+)
           :ret (spec/cat :gcd ::nat+ :coeffs (spec/tuple integer? integer?))
           :fn (fn [{{a :a b :b} :args {gcd :gcd [x0 x1] :coeffs} :ret}]
                 (= gcd (+' (*' x0 a) (*' x1 b)))))

(spec/fdef gcd
           :args (spec/cat :a ::nat+ :b ::nat+)
           :ret ::nat+
           :fn (fn [{{a :a b :b} :args gcd :ret}] (and (multiple? a gcd) (multiple? b gcd))))

(spec/fdef lcm
           :args (spec/cat :a ::nat+ :b ::nat+)
           :ret ::nat+
           :fn (fn [{{a :a b :b} :args lcm :ret}] (and (multiple? lcm a) (multiple? lcm b))))

(spec/fdef linear-diophantine
           :args (spec/cat :a ::nat+ :b ::nat+ :y ::nat+)
           :ret (spec/nilable (spec/cat :x0 integer? :x1 integer?))
           :fn #(fn [{{a :a b :b y :y} :args [x0 x1 :as ret] :ret}]
                  (or (nil? ret) (= (+' (*' x0 a) (*' x1 b)) y))))


(spec/fdef merge-meshes
           :args (spec/cat :a-mesh (spec/spec (spec/cat :a-base ::nat+ :a-shift integer?))
                           :b-mesh (spec/spec (spec/cat :b-base ::nat+ :b-shift integer?)))
           :ret (spec/nilable (spec/cat :base ::nat+ :shift integer?))
           :fn (fn [{{{a-base :a-base a-shift :a-shift} :a-mesh
                      {b-base :b-base b-shift :b-shift} :b-mesh} :args
                     [base shift :as ret]                        :ret}]
                 (or (nil? ret)
                     (and (multiple? (-' a-shift shift) a-base)
                          (multiple? (-' b-shift shift) b-base)
                          (multiple? base a-base)
                          (multiple? base b-base)))))

(spec/fdef find-theta
           :args (spec/cat :m ::nat+ :qs (spec/coll-of ::nat+) :index nat-int?)
           :ret ::nat+)
