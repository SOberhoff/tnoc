# tnoc
Clojure code inspired by The Nature of Computation by Cristopher Moore &amp; Stephan Mertens

If you are still unaware, [The Nature of Computation](http://nature-of-computation.org/) - hereby abbreviated TNOC - 
is an amazing book about computer science that deserves to be counted among other enduring works such as Structure and
Interpretation of Computer Programs or The Art of Computer Programming. This project is a collection of code that 
touches upon the topics laid out in TNOC. None of this is intended for production, but rather for exploration and experimentation.

## The Lambda Calculus

`tnoc.lambda` contains a number of functions for interpreting the lambda calculus devised by Alonzo Church in the 1930s. 
Lambda calculus forms are written in a [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) close to both Clojure
and the standard notation. The following examples will demonstrate:

`λ x. x` is written `(fn [x] x)`.

`λ x. x x` is written `(fn [x] (x x))` (Note that the parentheses that are silent in the first expression are mandatory in
the second.)

Forms may be curried: `(fn [x] (fn [y] (x y)))` is equivalent to `(fn [x y] (x y))`.

Church numerals can be abbreviated by integers. So `3` becomes `(fn [f x] (f (f (f x))))` during interpretation.

Forms can be referenced with symbols. So, if `SUCC` is a predefined successor function, `+2` can be defined as  
`(fn [n] (SUCC (SUCC n)))`. Symbols are substituted during interpretation as needed.

The namespace also contains a large number of predefined forms.

An example execution is:
```clojure
(println-reductions `(ADD 1 1))
0: (ADD 1 1)
1: ((fn [n m f x] (n f (m f x))) 1 1)
2: ((fn [m f x] (1 f (m f x))) 1)
3: (fn [f x] (1 f (1 f x)))
=> nil
```
This has reached normal form because the outermost form is now an abstraction and the standard rule is not to reduce the body
further before applying arguments. Also note the use of syntax quoting instead of regular quoting. This often leads to messier
interpretation traces but unqualified symbols can fail to resolve under certain circumstances. So be careful around those.
(For example lazy sequences can delay computation and cause symbol resolution to be performed in namespaces where the unqualified
symbol is no longer bound.)

### Simplify
If you want to play with fire you can try one of the simplifying functions.
```clojure
(println-reductions-simplified `(ADD 1 1))
0: (ADD 1 1)
1: (fn [f x] ((fn [f x] (f x)) f (1 f x)))
2: (fn [f x] ((fn [x] (f x)) (1 f x)))
3: (fn [f x] (f (1 f x)))
4: (fn [f x] (f ((fn [f x] (f x)) f x)))
5: (fn [f x] (f ((fn [x] (f x)) x)))
6: (fn [f x] (f (f x)))
=> nil
```
Beware that this may cause forms that would otherwise reach normal form to diverge:
```clojure
(println-reductions Y)
0: (fn [f] ((fn [x] (f (x x))) (fn [x] (f (x x)))))
1: (fn [f] ((fn [x] (f (x x))) (fn [x] (f (x x)))))
=> nil
```
 But:
 ```clojure
(println-reductions-simplified Y)
0: (fn [f] ((fn [x] (f (x x))) (fn [x] (f (x x)))))
1: (fn [f] (f ((fn [x] (f (x x))) (fn [x] (f (x x))))))
2: (fn [f] (f (f ((fn [x] (f (x x))) (fn [x] (f (x x)))))))
3: (fn [f] (f (f (f ((fn [x] (f (x x))) (fn [x] (f (x x))))))))
4: (fn [f] (f (f (f (f ((fn [x] (f (x x))) (fn [x] (f (x x)))))))))
5: (fn [f] (f (f (f (f (f ((fn [x] (f (x x))) (fn [x] (f (x x))))))))))
...
```
    
### Church Numerals

Church numerals can be manually converted from and to integers using `church` and `unchurch`. `unchurch` will simplify if the 
numeral isn't in the standard form.
```clojure
(normal-form `(EXP 2 4))
=> (fn [x] (2 (2 (2 (2 x)))))
```
```clojure
(normal-form-simplified `(EXP 2 4))
=> (fn [x G__10292] (x (x (x (x (x (x (x (x (x (x (x (x (x (x (x (x G__10292)))))))))))))))))
```
```clojure
(unchurch (normal-form-simplified `(EXP 2 4)))
=> 16
```
(The symbol `G__10292` was introduced at some point during the interpretation to avoid a collision when substituting `x`
for `f` in a form such as `(fn [f x] ...)`.)

### Differences to Clojure's evaluation model
While these forms look extremely similar to Clojure code, they aren't immediately executable through Clojure's `eval`. The
core differences are:
* Lambda forms are curried. `((fn [x y] x) a)` becomes `(fn [y] a)`, whereas Clojure will throw an `ArityException`.
* Lambda forms are lazy. `(Y (Y f) x)` substitutes `(Y f)` into `Y` at the next step, but Clojure will first evaluate `(Y f)`,
which in the case of the Y-combinator leads to infinite recursion.
* If you're using integers to stand for Church numerals they'll often result in forms such as `(0 x)`, which Clojure can't
evaluate because `0` isn't a function.


