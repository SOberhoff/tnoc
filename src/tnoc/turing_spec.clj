(in-ns 'tnoc.turing)

(spec/def ::direction #{:>> :<< :<>})

(spec/def ::transition (spec/cat :next-symbol char? :direction ::direction :next-state keyword?))

(spec/def ::transition-fn (spec/map-of char? ::transition))

(spec/def ::turing-machine (spec/map-of keyword? ::transition-fn))

(spec/def ::multi-transition (spec/cat :next-symbols string?
                                       :directions (spec/coll-of ::direction :kind vector?)
                                       :next-state keyword?))

(spec/def ::multi-transition-fn (spec/and (spec/map-of string? ::multi-transition)
                                          (fn [conformed]
                                            (every? #(= (count (ffirst conformed))
                                                        (count (first %))
                                                        (count ((second %) :next-symbols))
                                                        (count ((second %) :directions)))
                                                    conformed))))

(spec/def ::multi-turing-machine (spec/map-of keyword? ::multi-transition-fn))

