(in-ns 'tnoc.utils)

(spec/def ::nat+ (spec/and integer? (partial < 0)))

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

