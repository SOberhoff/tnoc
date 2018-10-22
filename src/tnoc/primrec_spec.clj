(in-ns 'tnoc.primrec)

(spec/def ::var (spec/and symbol? (comp not #{'_})))

(spec/def ::default-params (spec/coll-of symbol? :kind vector?))

(spec/def ::base-params
  (spec/spec (spec/cat :params (spec/* symbol?)
                       :last-param #{0})))

(spec/def ::recur-params
  (spec/spec
    (spec/cat :non-recur (spec/* symbol?)
              :recur (spec/or :S (spec/cat :S #{'S}
                                           :var (spec/and symbol? (comp not #{'_})))
                              :blank #{'_}))))

(spec/def ::body
  (spec/alt :zero #{0}
            :var symbol?
            :fn-call (spec/spec (spec/cat :fn symbol? :args (spec/* ::body)))))

(spec/fdef primrec
           :args (spec/cat :name symbol?
                           :clauses (spec/alt :default (spec/cat :default-params ::default-params
                                                                 :body ::body)
                                              :recur (spec/cat :base-params ::base-params
                                                               :base-body ::body
                                                               :recur-params ::recur-params
                                                               :recur-body ::body)))
           :ret any?)
