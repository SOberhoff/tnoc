(in-ns 'tnoc.edit-distance)

(spec/def ::string-or-char-seq (spec/or :string string? :char-seq (spec/coll-of char? :kind seq?)))

(spec/fdef edit-distance
           :args (spec/cat :s ::string-or-char-seq :t ::string-or-char-seq)
           :ret (spec/tuple nat-int? string? string?))