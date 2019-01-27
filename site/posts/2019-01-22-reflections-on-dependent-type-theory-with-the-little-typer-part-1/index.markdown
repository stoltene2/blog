---
title: Reflections on Dependent Type Theory with The Little Typer part 1
author: Eric Stolten
published: false
description: This series of posts is documents my journey through The Little Typer while I learn about Dependent Type Theory.
---

The book, [The Little Typer](http://www.thelittletyper.com/), is about
Dependent Type Theory (DTT). The format is consistent with other "The
Little" books. I'm not a stranger to type theory, but I've never had a
good introduction to DTT.

As I journey through the book I am going to post small problems I have
completed in addition to other functions I found interesting to
implement.

So far, I've found this book to be a great introduction to types. If
you've ever wanted to understand the difference between Types, Type
Constructors, and Data constructors then you should read this
book. When coming from an untyped language or a simply typed language
it isn't intuitive how to reason about types. This book sets the stage
for reasoning about types. Because of the constructive nature of the
book it is really clear that if you have an `Atom` then functions
which take `Atom`s as an argument can only work within the world of
`Atom`s.

## Goals of this post

* Describe how to create the `factorial` function and determine its
  types in the dependently typed language [Pie](https://docs.racket-lang.org/pie/index.html).

## The fun of factorial

Factorial is an easy function to implement in most languages and it is
familiar to many. While it does not need Dependent Type Theory it will
get our feet wet with Pie.

In the [Pie Language](https://docs.racket-lang.org/pie/index.html),
all dependencies need to be claimed and defined before they are
used. _Claiming_ is stating the type and _Defining_ is the
implementation.

Let's derive the factorial function. With recursion this is easy to
write. However, Pie does not have explicit recursion. We need to use
other built-in functions. The language contains functions which allow
us to destructure or _eliminate_ types to produce smaller types. While
eliminating, we have access to functions like `rec-Nat`, recursive
Nat, which help us evaluate a step function for every successively
smaller natural number until we reach `zero`.

Mathematically, the factorial function is determined by, $n! = 1*2*3*...*(n-1)*n$.

This indicates the type needed for factorial. It takes a [natural
number](https://en.wikipedia.org/wiki/Natural_number), `Nat`, and
returns another `Nat`. The type solidifies that our function does
nothing more than this. Because factorial only knows about consuming
Nat and must return a Nat we have limited tools at our
disposal. Namely, any function which operates on `Nat`s.

~~~ {.lisp .html}
(claim factorial
  (-> Nat
      Nat))
~~~

Because `factorial` successively multiplies smaller Nats together we
can use the `rec-Nat` function.[^1]

[^1]: This is defined in the top down order and contains some work up to this point.

From the documentation of `rec-Nat` in [Pie](https://docs.racket-lang.org/pie/index.html#%28def._%28%28lib._pie%2Fmain..rkt%29._rec-.Nat%29%29),

~~~ {.lisp .html}
(rec-Nat target base step) → X
  target : Nat
  base : X
  step : (-> Nat X X)
~~~

The `base` case of `factorial` needs to return `1` when its `target`
is `0`. We can deduce that `X = Nat` in the above definition. Here is our Claim and Definition of factorial with the substitution `X = Nat`. When this is done automatically by a type-checker it is called [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science)).

~~~ {.lisp .html}
(claim factorial
  (-> Nat
      Nat))

(define factorial
  (λ (n)
    (rec-Nat n
      1
      step-factorial)))
~~~

Based on the substitution above the type of `step-factorial` will need
to take two `Nat`s as arguments and returns a `Nat` based on the above
usage of `rec-Nat`.

To see clearly our definition of `step-factorial`, `rec-Nat` with
`X=Nat` is clear.

~~~ {.lisp .html}
(rec-Nat target base step) → Nat
  target : Nat
  base : Nat
  step : (-> Nat Nat Nat)
~~~

~~~ {.lisp .html}
(claim step-factorial
  (-> Nat Nat
      Nat))
~~~

Finally, we can determine the whole function in order.[^2]

[^2]: This assumes you have defined * already

~~~ {.lisp .html}
(claim step-factorial
  (-> Nat Nat
      Nat))

(define step-factorial
  (λ (n-1 fact-1)
    (* (add1 n-1) fact-1)))

(claim factorial
  (-> Nat
      Nat))

(define factorial
  (λ (n)
    (rec-Nat n
      1
      step-factorial)))
~~~

## Showing our work

We can see the in the same-as chart as listed in equal forms.

~~~ {.lisp .html}
(factorial 2)

;; Same as

(rec-Nat 2
    1
    step-factorial)

;; Same as

(step-factorial
  1
  (rec-Nat 1
    1
    step-factorial))

;; Same as

((λ (n-1 fact-1)
    (* (add1 n-1) fact-1))

    1

    (rec-Nat 1
      1
      step-factorial))

;; Same as

(* (add1 1)
   (rec-Nat 1
     1
     step-factorial))

;; Same as

(* (add1 1) (add1 zero))

;; Same as

2

~~~

Stay tuned for more updates on Dependent Type Theory.
