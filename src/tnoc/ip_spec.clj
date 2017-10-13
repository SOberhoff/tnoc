(in-ns 'tnoc.ip)

(spec/def ::variable-power (spec/tuple keyword? nat-int?))

(spec/def ::term (spec/* (spec/or :coefficient integer? :variable-power ::variable-power)))

(spec/def ::sum (spec/cat :mult #{'+} :operands (spec/* ::polynomial)))

(spec/def ::product (spec/cat :mult #{'*} :operands (spec/* ::polynomial)))

(spec/def ::polynomial (spec/or :term ::term
                                :sum ::sum
                                :product ::product))

(spec/def ::literal (spec/or :constant boolean? :variable keyword?))

(spec/def ::negation (spec/cat :not #{'not} :clause ::formula))

(spec/def ::conjunction (spec/cat :and #{'and} :clauses (spec/* ::formula)))

(spec/def ::disjunction (spec/cat :or #{'or} :clauses (spec/* ::formula)))

(spec/def ::all (spec/cat :A #{'A} :variable keyword? :body ::formula))

(spec/def ::exists (spec/cat :E #{'E} :variable keyword? :body ::formula))

(spec/def ::formula (spec/or
                      :literal ::literal
                      :negation ::negation
                      :conjuction ::conjunction
                      :disjuction ::disjunction
                      :all ::all
                      :exists ::exists))
