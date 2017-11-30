(in-ns 'tnoc.utils)

(spec/def ::nat+ (spec/and integer? (partial < 0)))

(spec/fdef log
           :args (spec/cat :b number? :x number?)
           :ret number?)

(spec/fdef next-int
           :args (spec/cat :x number?)
           :ret integer?
           :fn (fn [{{x :x} :args ret :ret}] (<= x ret)))

(spec/fdef multiple?
           :args (spec/cat :a integer? :b integer?)
           :ret boolean?)

(spec/fdef euclid
           :args (spec/cat :a ::nat+ :b ::nat+)
           :ret (spec/tuple integer? integer? ::nat+)
           :fn (fn [{{a :a b :b} :args [x y gcd] :ret}]
                 (= gcd (+' (*' x a) (*' y b)))))

(spec/fdef gcd
           :args (spec/cat :a ::nat+ :b ::nat+)
           :ret ::nat+
           :fn (fn [{{a :a b :b} :args gcd :ret}] (and (multiple? a gcd) (multiple? b gcd))))

(spec/fdef lcm
           :args (spec/cat :a ::nat+ :b ::nat+)
           :ret ::nat+
           :fn (fn [{{a :a b :b} :args lcm :ret}] (and (multiple? lcm a) (multiple? lcm b))))

(spec/fdef rand-bigint
           :args (spec/cat :n ::nat+)
           :ret integer?)

(spec/fdef miller-rabin
           :args (spec/cat :p integer?)
           :ret boolean?)

(spec/fdef prime?
           :args (spec/cat :p integer?)
           :ret boolean?)



