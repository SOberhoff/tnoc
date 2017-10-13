(in-ns 'tnoc.ip)

(spec/def ::variable (spec/cat :base keyword? :power nat-int?))

(spec/def ::term (spec/cat :coefficient integer? :variable (spec/* (spec/spec ::variable))))

(spec/def ::sum (spec/cat :mult #{'+} :operands (spec/* ::polynomial)))

(spec/def ::product (spec/cat :mult #{'*} :operands (spec/* ::polynomial)))

(spec/def ::polynomial (spec/or :term ::term
                                :sum ::sum
                                :product ::product))
