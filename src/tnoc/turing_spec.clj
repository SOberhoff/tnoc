(in-ns 'tnoc.turing)

(spec/def ::direction #{:>> :<< :<>})

(spec/def ::transition (spec/cat :next-symbol char? :direction ::direction :next-state keyword?))

(spec/def ::transition-fn (spec/map-of char? ::transition))

(spec/def ::turing-machine (spec/map-of keyword? ::transition-fn))

(spec/def ::transition' (spec/cat :next-symbol (spec/? char?) :direction ::direction :next-state (spec/? keyword?)))

(spec/def ::transition-fn' (spec/map-of char? ::transition'))

(spec/def ::turing-machine' (spec/map-of keyword? ::transition-fn'))

