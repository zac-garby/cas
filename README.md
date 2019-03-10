# cas

A computer algebra system in Haskell. At the time of writing this it consists of only 85 lines of code.

## Example

As an example, some expansion rules are defined in [Main.hs](Main.hs). Using these, the expression `tan(X + Y)` can be expanded into the following forms:

```
tan((X + Y))
(sin((X + Y)) / cos((X + Y)))
(((sin(X) * cos(Y)) + (cos(X) * sin(Y))) / cos((X + Y)))
(sin((X + Y)) / ((cos(X) * cos(Y)) - (sin(X) * sin(Y))))
(((sin(X) * cos(Y)) + (cos(X) * sin(Y))) / ((cos(X) * cos(Y)) - (sin(X) * sin(Y))))
```

## What needs doing?

 - Add unary prefix operators (e.g. `-x`, `!y`).
 - Write more transformation rules.
     - Basic axioms like `a + b = b + a`, `a + 0 = a`.
     - More trigonemtry identities.
     - Loads of other things.
     - Might be a good idea to transform `a - b` into `a + (-b)` and similarly `a / b` into `a * (1/b)`.
 - Simplification of expressions.
     - Might be implemented as another set of transformation rules, constructed from some designed to simplify but also the inverses of some expansion rule sets.
 - Parsing of expressions, so things can be entered much more easily.
 - A REPL.
 - Abstract a lot of behaviour by implementing a functor-like interface for terms.