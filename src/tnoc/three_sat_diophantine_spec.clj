(in-ns 'tnoc.three-sat-diophantine)

(spec/def ::var (spec/with-gen keyword? #(spec/gen #{:a :b :c :d :e :f})))

(spec/def ::literal (spec/or :var ::var :negated (spec/cat :not #{'!} :var ::var)))

(spec/def ::clause (spec/tuple ::literal ::literal ::literal))

(spec/def ::formula (spec/coll-of ::clause :kind seq? :gen-max 5))