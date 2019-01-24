---
title: Reflections on Dependent Type Theory with The Little Typer part 1
author: Eric Stolten
published: false
description: This series of posts is documents my journey through The Little Typer while I learn about Dependent Type Theory.
---

The Little Typer Book is a book about Dependent Type Theory (DTT). The
format is consistent with the other "Little" books. I'm not a stranger
to type theory, but I was always left scratching my head when it came
to DTT.

As I journey through the book I am going to post small problems I have
completed in addition to other functions I found interesting to
implement.

## Goals of this post

* Describe how to create the `factorial` function and determine its
  types in the dependently typed language [Pie](https://docs.racket-lang.org/pie/index.html).

## The fun of factorial

In the [Pie Language](https://docs.racket-lang.org/pie/index.html),
everything needs to be defined before it is used. This is because the
language is simple, educational, and does not require this
complexity. In addition, you will not see any form of module
system. In my examples, I may define snippets top-down instead of
bottom-up.

Let's come up with the factorial function in Pie. With recursion this
is easy to write. However, we do not have recursion built into Pie. We
have functions which allow us to destructure or _eliminate_
types. While eliminating we have functions like `rec-Nat`, recursive
Nat, which help us achieve the same result.

When we want to derive a factorial function we start with its
definition.

The factorial function is determined by, $n! = 1*2*3*...*(n-1)*n$.

This indicates the type of factorial. It takes a [natural number]() and
returns another natural number. The type solidifies that it does nothing more
than that.

~~~ {.lisp .html}
(claim factorial
  (-> Nat
      Nat))
~~~

We know that we want to accumulate of the final answer. We also know
that we need to keep track of the result of the last
computation. Because of this we will use `rec-Nat` to calculate the
factorial function.[^1]

TODO: Review the above. Not sure it makes clear sense.

The base case of `factorial` needs to return `1` when its input is
`0`.

[^1]: This is defined in the top down order and contains some work up to this point.

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

Now, we need to determine the type of `step-factorial` and define the
function. We know from the Laws of `rec-Nat` that the return result of
`step-factorial` needs to have the same type of our _base_, `1`. We
know from the definition of `rec-Nat` provided in [The Little Typer](https://mitpress.mit.edu/books/little-typer)
that the _step_ function takes two arguments. An `n-1` value and the
next invocation of `rec-Nat`.

We can determine the type of `step-factorial` to take two Nats as
arguments and returns a Nat.

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

## Show your work

We can see the in the same-as chart as listed in equal forms.

~~~ {.lisp .html}
(factorial 2)

;; Same as

(rec-Nat 2
    1
    step-factorial)

;; Same as

(step-factorial
  2
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
;; TODO: keep it going
((λ (n-1 fact-1)
    (* (add1 n-1) fact-1))

    1

    (rec-Nat 1
      1
      step-factorial))

~~~
