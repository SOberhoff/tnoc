(in-ns 'tnoc.ip)

(spec/def ::term (spec/tuple integer? (spec/map-of keyword? nat-int?)))

(spec/def ::Sum
  (spec/with-gen (partial instance? Sum)
                 #(sgen/fmap (fn [polynomials] (->Sum polynomials))
                             (sgen/vector (spec/gen ::polynomial)))))

(spec/def ::Product
  (spec/with-gen (partial instance? Product)
                 #(sgen/fmap (fn [polynomials] (->Product polynomials))
                             (sgen/vector (spec/gen ::polynomial)))))

(spec/def ::polynomial (spec/or :term ::term
                                :Sum ::Sum
                                :Product ::Product))

(spec/def ::literal (spec/or :constant boolean? :variable keyword?))

(spec/def ::negation (spec/cat :not #{'not} :clause ::formula))

(spec/def ::conjunction (spec/cat :and #{'and} :clauses (spec/* ::formula)))

(spec/def ::disjunction (spec/cat :or #{'or} :clauses (spec/* ::formula)))

(spec/def ::forall (spec/cat :A #{'A} :variable keyword? :body ::formula))

(spec/def ::exists (spec/cat :E #{'E} :variable keyword? :body ::formula))

(spec/def ::reduced (spec/cat :R #{'R} :variable keyword? :body ::formula))

(spec/def ::quantified (spec/or :forall ::forall :exists ::exists :reduced ::reduced))

(spec/def ::formula (spec/or
                      :literal ::literal
                      :negation ::negation
                      :conjuction ::conjunction
                      :disjuction ::disjunction
                      :quantified ::quantified))

(spec/fdef ->Sum
           :args (spec/cat :polynomials (spec/coll-of ::polynomial :min-count 1))
           :ret ::Sum)

(spec/fdef ->Product
           :args (spec/cat :polynomials (spec/coll-of ::polynomial :min-count 1))
           :ret ::Product)

(spec/fdef group-by-polynomials
           :args (spec/cat :polynomials (spec/coll-of ::polynomial))
           :ret (spec/map-of keyword? (spec/coll-of ::polynomial)))

(spec/fdef get-degree
           :args (spec/cat :term ::term)
           :ret nat-int?)

(spec/fdef sort-polynomials
           :args (spec/cat :polynomials (spec/coll-of ::polynomial))
           :ret (spec/coll-of ::polynomial))

(spec/fdef make-sum
           :args (spec/cat :polynomials (spec/coll-of ::polynomial))
           :ret ::polynomial)

(spec/fdef make-product
           :args (spec/cat :polynomials (spec/coll-of ::polynomial))
           :ret ::Product)

(spec/fdef simplify-term
           :args (spec/cat :term ::term)
           :ret ::term)

(spec/fdef multiply-terms
           :args (spec/cat :term-0 ::term :term-1 ::term)
           :ret ::term)

(spec/fdef add
           :args (spec/cat :sum ::Sum)
           :ret ::polynomial)

(spec/fdef multiply
           :args (spec/cat :product ::Product :distributive? boolean?)
           :ret ::polynomial)

(spec/fdef simplify
           :args (spec/cat :polynomial ::polynomial :distributive? boolean?)
           :ret ::polynomial)

(spec/fdef substitute
           :args (spec/cat :polynomial ::polynomial :substitution (spec/map-of keyword? integer?))
           :ret ::polynomial)

(spec/fdef arithmetic-negate
           :args (spec/cat :polynomial ::polynomial)
           :ret ::polynomial)

(spec/fdef arithmetize
           :args (spec/cat :formula ::formula)
           :ret ::polynomial)

(spec/fdef serialize-term
           :args (spec/cat :term ::term)
           :ret string?)

(spec/fdef serialize
           :args (spec/cat :polynomial ::polynomial)
           :ret string?)
