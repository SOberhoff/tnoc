(in-ns 'tnoc.ip)

(spec/def ::alg-literal (spec/or :number integer? :variable keyword?))

(spec/def ::exponentiation (spec/cat :operator #{'**}
                                     :base ::polynomial
                                     :exponent (spec/with-gen nat-int? #(sgen/choose 0 5))))

(spec/def ::product (spec/cat :operator #{'*} :operands (spec/+ ::polynomial)))

(spec/def ::sum (spec/cat :operator #{'+} :operands (spec/+ ::polynomial)))

(spec/def ::polynomial (spec/or :literal ::alg-literal
                                :exponentiation ::exponentiation
                                :product ::product
                                :sum ::sum))

(spec/def ::polynomials (spec/coll-of ::polynomial :kind sequential?))

(spec/def ::bool-literal (spec/or :boolean boolean? :variable keyword?))

(spec/def ::negation (spec/cat :not #{'not} :clause ::formula))

(spec/def ::conjunction (spec/cat :and #{'and} :clauses (spec/* ::formula)))

(spec/def ::disjunction (spec/cat :or #{'or} :clauses (spec/* ::formula)))

(spec/def ::forall (spec/cat :A #{'A} :variable keyword? :body ::formula))

(spec/def ::exists (spec/cat :E #{'E} :variable keyword? :body ::formula))

(spec/def ::reduced (spec/cat :R #{'R} :variable keyword? :body ::formula))

(spec/def ::quantified (spec/or :forall ::forall :exists ::exists :reduced ::reduced))

(spec/def ::formula (spec/or
                      :literal ::bool-literal
                      :negation ::negation
                      :conjuction ::conjunction
                      :disjuction ::disjunction
                      :quantified ::quantified))

(spec/def ::substitutions (spec/map-of keyword? integer?))

(spec/def ::interaction (spec/tuple ::formula ::polynomial ::polynomial boolean?))

(spec/fdef make-sum
           :args (spec/cat :terms ::polynomials)
           :ret ::polynomial)

(spec/fdef make-product
           :args (spec/cat :factors ::polynomials)
           :ret ::polynomial)

(spec/fdef coefficient-rem-factors
           :args (spec/cat :factors ::polynomials)
           :ret (spec/tuple integer? ::polynomials))

(spec/fdef merge-factors
           :args (spec/cat :factors ::polynomials)
           :ret ::polynomial)

(spec/fdef merge-terms
           :args (spec/cat :terms ::polynomials)
           :ret ::polynomial)

(spec/fdef add
           :args (spec/cat :polynomial-1 ::polynomial
                           :polynomial-2 ::polynomial)
           :ret ::polynomial)

(spec/fdef multiply
           :args (spec/cat :polynomial-1 ::polynomial
                           :polynomial-2 ::polynomial
                           :distributive? boolean?)
           :ret ::polynomial)

(spec/fdef simplify
           :args (spec/or :arity-1 (spec/cat :polynomial ::polynomial)
                          :arity-2 (spec/cat :polynomial ::polynomial
                                             :distributive? boolean?))
           :ret ::polynomial)

(spec/fdef arithmetic-negate
           :args (spec/cat :polynomial ::polynomial)
           :ret ::polynomial)

(spec/fdef arithmetic-forall
           :args (spec/cat :polynomial ::polynomial
                           :variable keyword?)
           :ret ::polynomial)

(spec/fdef arithmetic-exists
           :args (spec/cat :polynomial ::polynomial
                           :variable keyword?)
           :ret ::polynomial)

(spec/fdef arithmetic-reduced
           :args (spec/cat :polynomial ::polynomial
                           :variable keyword?)
           :ret ::polynomial)

(spec/fdef arithmetize
           :args (spec/cat :formula ::formula)
           :ret ::polynomial)

(spec/fdef arithmetize-simplifying
           :args (spec/cat :formula ::formula)
           :ret ::polynomial)

(spec/fdef merlin
           :args (spec/cat :formula ::formula
                           :substitutions ::substitutions)
           :ret (spec/tuple ::polynomial (spec/nilable ::polynomial)))

(spec/fdef arthur
           :args (spec/cat :formula ::formula
                           :claim ::polynomial
                           :proof (spec/nilable ::polynomial)
                           :substitutions ::substitutions)
           :ret boolean?)

(spec/fdef free-variables
           :args (spec/cat :formula ::formula)
           :ret (spec/coll-of keyword? :kind set?))

(spec/fdef interact-once
           :args (spec/cat :formula ::formula
                           :substitutions ::substitutions)
           :ret ::interaction)

(spec/fdef interact
           :args (spec/cat :formula ::formula
                           :test-ints (spec/coll-of integer? :kind sequential?))
           :ret (spec/coll-of ::interaction :kind sequential?))

(spec/fdef pprint-polynomial
           :args (spec/cat :polynomial (spec/nilable ::polynomial))
           :ret string?)

(spec/fdef pprint-interaction
           :args (spec/cat :interactions (spec/+ ::interaction))
           :ret (spec/or :pprinted-interaction ::interaction
                         :pprinted-interactions (spec/coll-of ::interaction :kind sequential?)))

(spec/fdef interact-manually
           :args (spec/cat :formula ::formula)
           :ret nil?)

(spec/fdef remove-reductions
           :args (spec/cat :formula ::formula)
           :ret ::formula)

(spec/fdef add-interactions
           :args (spec/cat :formula ::formula)
           :ret ::formula)
