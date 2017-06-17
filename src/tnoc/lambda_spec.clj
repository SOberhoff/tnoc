(spec/def ::symbol (spec/with-gen symbol? #(gen/fmap (comp symbol str) (spec/gen char?))))

(spec/def ::params (spec/coll-of ::symbol, :kind vector?, :gen-max 3))

(spec/def ::abstraction (spec/cat :fn #{'fn `fn}, :params ::params, :body ::form))

(spec/def ::combinator (spec/or :symbol ::symbol, :church nat-int?, :abstraction ::abstraction))

(spec/def ::nested-form (spec/coll-of ::form, :kind seq?, :min-count 1, :gen-max 3))

(spec/def ::form (spec/or :combinator ::combinator, :nested-form ::nested-form))

(spec/fdef substitute
           :args (spec/cat :body ::nested-form, :param ::symbol, :argument ::form)
           :ret ::form)

(spec/fdef apply-abstraction
           :args (spec/cat :operator ::abstraction, :operand ::form)
           :ret ::form)

(spec/fdef church
           :args (spec/cat :nat-int nat-int?)
           :ret ::abstraction)

(spec/fdef final-abs-application?
           :args (spec/cat :form ::form)
           :ret boolean?)

(spec/fdef non-final-abs-application?
           :args (spec/cat :form ::form)
           :ret boolean?)

(spec/fdef get-symbol
           :args (spec/cat :symbol ::symbol)
           :ret (spec/nilable ::form))

(spec/fdef normalize
           :args (spec/cat :form ::form, :fully? boolean?)
           :ret ::form)

(spec/fdef normalize-eager
           :args (spec/cat :form ::form, :fully? boolean?)
           :ret ::form)

(spec/fdef normal-form
           :args (spec/cat :form ::form)
           :ret ::form)

(spec/fdef normal-form-eager
           :args (spec/cat :form ::form)
           :ret ::form)

(spec/fdef normal-form-reductions
           :args (spec/cat :form ::form)
           :ret (spec/coll-of ::form, :kind seq?, :min-count 1))

(spec/fdef normal-form-reductions-eager
           :args (spec/cat :form ::form)
           :ret (spec/coll-of ::form, :kind seq?, :min-count 1))

(spec/fdef println-reductions
           :args (spec/cat :form ::form)
           :ret nil?)

(spec/fdef println-reductions-eager
           :args (spec/cat :form ::form)
           :ret nil?)