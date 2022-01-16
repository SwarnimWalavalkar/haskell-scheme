# A Scheme Implementation in Haskell

Refrence: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/

To run:

```bash
$ cabal run scheme
```

### Examples:

```
Lisp>>> (+ 2 3)
5

Lisp>>> (cons 2 3)
(2 . 3)

Lisp>>> (cons 'this '())
(this)
```

```
Lisp>>> (cdr '(a simple test))
(simple test)

Lisp>>> (car '((this is) a test))
(this is)

Lisp>>> (cons '(this is) 'test)
((this is) . test)

Lisp>>> (eqv? 1 3)
#f

Lisp>>> (eqv? 3 3)
#t
```
