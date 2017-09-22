(in-ns 'tnoc.rsa)

(spec/def ::bigint (partial instance? BigInt))

; like nat-int? but also accepts BigInts
(spec/def ::nat (spec/with-gen (spec/and integer? #(<= 0 %))
                               #(sgen/gen-for-pred nat-int?)))

; like pos-int? but also accepts BigInts
(spec/def ::nat+ (spec/with-gen (spec/and integer? #(< 0 %))
                                #(sgen/gen-for-pred pos-int?)))

; like string? but excludes the empty string
(spec/def ::string+ (spec/and string?
                              #(< 0 (.length %))))

(spec/fdef string-to-bigints
           :args (spec/cat :string string? :bytesize pos-int?)
           :ret (spec/coll-of ::bigint :kind seq?))

(spec/fdef bigints-to-string
           :args (spec/coll-of ::bigint :kind seq?)
           :ret string?)

(spec/fdef mod-exp
           :args (spec/cat ::x ::nat ::y ::nat ::n ::nat)
           :ret ::nat)

(spec/fdef euclid
           :args (spec/cat :a ::nat+ :b ::nat+)
           :ret (spec/cat :x integer? :y integer? :gcd ::nat+)
           :fn (fn [{{a :a b :b}          :args
                     {x :x y :y gcd :gcd} :ret}]
                 (and (zero? (mod a gcd))
                      (zero? (mod b gcd))
                      (= (+' (*' x a) (*' y b)) gcd))))

(spec/fdef invert
           :args (spec/and (spec/cat :a ::nat+ :n ::nat+)
                           #(< (:a %) (:n %))
                           #(= 1 (gcd (:a %) (:n %))))
           :ret ::nat+
           :fn (fn [{{a :a n :n} :args
                     ret         :ret}]
                 (= 1 (mod (*' a ret) n))))

(spec/fdef rand-bigint
           :args (spec/and (spec/cat :n ::nat+)
                           #(< 1 (:n %)))
           :ret ::nat+
           :fn #(< 0 (:ret %) (:n (:args %))))

(spec/fdef miller-rabin
           :args (spec/cat :p ::nat)
           :ret (spec/nilable boolean?))

(spec/fdef prime?
           :args (spec/cat :n ::nat)
           :ret boolean?)

(spec/fdef probable-prime
           :args (spec/or :arity-1 (spec/cat :bitsize pos-int?)
                          :arity-2 (spec/cat :bitsize pos-int? :e ::nat+))
           :ret ::nat+)

(spec/fdef make-keys
           :args (spec/cat :bitsize pos-int? :e ::nat+)
           :ret (spec/keys :req-un [::p ::q ::N ::e]))

(spec/fdef encrypt
           :args (spec/and (spec/cat :string ::string+ :N ::nat+ :e ::nat+)
                           #(<= 16 (.bitLength (bigint (:N %)))))
           :ret (spec/coll-of ::nat :kind seq? :min-count 1))

(spec/fdef encrypt-with-new-keys
           :args (spec/cat :string ::string+ :bitsize pos-int? :e ::nat+)
           :ret (spec/coll-of ::nat :kind seq? :min-count 1))

(spec/fdef decrypt
           :args (spec/cat :encryption (spec/coll-of ::nat :kind seq? :min-count 1) :p ::nat+ :q ::nat+ :e ::nat+)
           :ret ::string+)
