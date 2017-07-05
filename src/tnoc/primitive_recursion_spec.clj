(in-ns 'tnoc.primitive-recursion)

(spec/def ::base-params (spec/cat :params (spec/* symbol?)
                                  :last-param (spec/? #{0})))

(spec/def ::recur-params (spec/cat :non-recur (spec/* symbol?)
                                   :recur (spec/? (spec/spec (spec/cat :S #{'S} :var symbol?)))))

(spec/def ::body (spec/alt :zero #{0}
                           :var symbol?
                           :fn-call (spec/spec (spec/cat :fn symbol? :args (spec/* ::body)))))

(spec/fdef primrec
           :args (spec/cat :name symbol?
                           :base-params (spec/spec ::base-params)
                           :base-body ::body
                           :recur (spec/? (spec/cat :recur-params (spec/spec ::recur-params)
                                                    :recur-body ::body)))
           :ret any?)
