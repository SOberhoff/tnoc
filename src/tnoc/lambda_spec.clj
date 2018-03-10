(in-ns 'tnoc.lambda)

(spec/def ::symbol (spec/with-gen symbol? #(sgen/fmap (comp symbol str) (spec/gen char?))))

(spec/def ::params (spec/coll-of ::symbol, :kind vector?, :distinct true, :gen-max 3))

(spec/def ::abstraction (spec/cat :fn #{'fn `fn}, :params ::params, :body ::form))

(spec/def ::nested-form (spec/coll-of ::form, :kind seq?, :min-count 1, :gen-max 3))

(spec/def ::form (spec/or :symbol ::symbol
                          :church nat-int?
                          :abstraction ::abstraction
                          :nested-form ::nested-form))

(spec/fdef substitute
           :args (spec/cat :body ::nested-form, :param ::symbol, :argument ::form)
           :ret ::form)

(spec/fdef fuzz-parameter
           :args (spec/cat :abstraction ::abstraction, :param ::symbol)
           :ret ::abstraction)

(spec/fdef free-variables
           :args (spec/cat :form ::form)
           :ret (spec/coll-of ::symbol :kind set?))

(spec/fdef apply-abstraction
           :args (spec/cat :operator ::abstraction, :operand ::form)
           :ret ::form)

(spec/fdef church
           :args (spec/cat :nat-int nat-int?)
           :ret ::abstraction)

(spec/fdef final-apply?
           :args (spec/cat :form ::form)
           :ret boolean?)

(spec/fdef non-final-apply?
           :args (spec/cat :form ::form)
           :ret boolean?)

(spec/fdef get-symbol
           :ret (spec/nilable ::form))

(spec/fdef normalize-once
           :args (spec/cat :form ::form)
           :ret ::form)

(spec/fdef normalize
           :args (spec/cat :form ::form)
           :ret ::form)

(spec/fdef normalize-simplified-once
           :args (spec/cat :form ::form)
           :ret ::form)

(spec/fdef normalize-simplified
           :args (spec/cat :form ::form)
           :ret ::form)

(spec/fdef normalize-reductions
           :args (spec/cat :form ::form)
           :ret (spec/coll-of ::form, :kind seq?, :min-count 1))

(spec/fdef normalize-simplified-reductions
           :args (spec/cat :form ::form)
           :ret (spec/coll-of ::form, :kind seq?, :min-count 1))

(spec/fdef println-reductions
           :args (spec/cat :form ::form)
           :ret nil?)

(spec/fdef println-reductions-simplified
           :args (spec/cat :form ::form)
           :ret nil?)

(spec/fdef unchurch
           :args (spec/cat :church-numeral ::abstraction)
           :ret integer?)

(spec/fdef resolve-references
           :args (spec/cat :form ::form)
           :ret ::form)
