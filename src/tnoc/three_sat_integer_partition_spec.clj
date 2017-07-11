(in-ns 'tnoc.three-sat-integer-partition)

(spec/def ::var (spec/with-gen keyword? #(spec/gen #{:a :b :c :d})))

(spec/def ::negated-var (spec/with-gen #(instance? Not %) #(sgen/fmap ! (spec/gen ::var))))

(spec/def ::literal (spec/or :var ::var :negated-var ::negated-var))

(spec/def ::clause (spec/tuple ::literal ::literal ::literal))

(spec/def ::formula (spec/coll-of ::clause :kind vector? :gen-max 5))
