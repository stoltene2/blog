---
title: Replicate a List in Pie - The Little Typer Part 2
author: Eric Stolten
published: false
description: In Part 2, we will implement list replication
---

[In
continuation](../2019-01-27-reflections-on-dependent-type-theory-with-the-little-typer-part-1/index.html)
with my exploration of The Little Typer I'm going to implement
`replicate` on `List`s. The replicate function is [pretty standard in
Haskell](https://www.stackage.org/haddock/lts-13.5/base-4.12.0.0/Prelude.html#v:replicate). Our
goal here is to create a similar function in the dependently typed
programming language
[Pie](https://docs.racket-lang.org/pie/index.html#%28def._%28%28lib._pie%2Fmain..rkt%29._rec-.Nat%29%29).

This post still does not utilize the power of Dependent Types. Instead
this is uses what I'm referring to as standard types.

There is a difference between writing `replicate` with standard types
and dependent types. With dependent types we are able to encode the
length of the list in the type. We'll get to that soon enough and this
post will help us get there.

## Goals of this post

* Show how to create a function `replicate` which takes a number, `n`,
  of type `Nat`, an element of any type `E`, and return a list of type
  `List E` with length `n`.

* Introduce ∏ expressions, also written as _Pi_ expressions, and
  describe why they are needed.

## Primer on `List E`

Here are a few operations we can perform with `List E` types.

- Construct values of type `List E` using the constructor `(:: e es)`
  where `e` has type `E` and `es` has type `List E`.
- Construct empty lists with the empty list constructor `nil`.

The last point above, requires clarification. The compiler needs to
know which type of `nil` you are using with a type annotation. This is
needed because there exists a `nil` value for each type `List E`.  For
example, an empty list of `Nat`s can be constructed using `(the (List
Nat) nil)`. The annotation tells the compiler which type we mean.

## How we want replicate to work

When we call replicate we want to provide the following arguments,

- The type of the element we want to replicate
- The element we want to replicate, could be an element of _any_ type.
- The number of times to replicate that element

For example, to create a list of natural numbers, `List Nat`, where
the `Nat` 3 is repeated 3 times, we would have the following.

~~~ {.lisp .html}
(list-replicate Nat 3 3)

;; Results in

(the (List Nat)
  (:: 3
    (:: 3
      (:: 3 nil))))
~~~

If you are not familiar with Pie then the expression `(the Type
value)` may look funny to you. The function `the` serves as a type
annotation for values. In the case above it told us the
`list-replicate` had a return type of `(List Nat)` and its value in
normal form is a list with 3 elements where all the elements are
`Nat`s of value 3. This falls in line with annotating the type of `nil`.

Using the same function we can create a list of lists. If we wanted to
take the list `(:: 'banana (:: 'mangos nil))` and replicate it 4 times
because they are delicious we would have the following.

~~~ {.lisp .html}
(list-replicate (List Atom) 4 (:: 'banana (:: 'mangos nil)))

(the (List (List Atom))
  (:: (:: 'banana (:: 'mangos nil))
    (:: (:: 'banana (:: 'mangos nil))
      (:: (:: 'banana (:: 'mangos nil))
        (:: (:: 'banana (:: 'mangos nil))
          nil)))))
~~~

From this we can see the resulting type `(List (List Atom))` which as
a value is a list of lists.

## I want something from the Universe.

Although it isn't mentioned explicitly in the book we want to create
polymorphic functions which operate on `List E` for any `E`. When you
read or say this it is understood that we mean for any E that is a
type. For example, `E` could be a `Nat`, `Atom`, or `List (List (List
Nat))`. We know what we mean, but the compiler has no idea.

We need to tell the _Pie_ compiler what we mean about `E`. Here is
what happens if we do not inform the compiler. Consider an identity
function, `id`. We'd like to say that it takes an `E` and returns an
`E`. However, the type `E` is unknown to the compiler.

~~~ {.lisp .html}
> (claim id
    (-> E
      E))

"Unknown variable E"
~~~

### ∏ expressions

Enter ∏-expressions, or Pi expression. By using Pi expressions with the
following form we can declare `E` to be some type in the
_body_. `U` is the universe of types and all[^2] types introduced in
_Pie_ live inside there.

[^2]: Though there are infinitely many types. You can see this by
    considering what universe `U` belongs to.

~~~ {.lisp .html}
(Pi ((E U))
  body)
~~~

Now, we are prepared to write the `id` function.

~~~ {.lisp .html}
(claim id
  (Pi ((E U))
    (-> E
      E)))

(define id
  (λ (E e)
    e))
~~~

In the above code, notice that the lambda function `(λ (E e) e)`
accepts two arguments when the claim only defined one argument. This
is because of the Pi expression. Types declared in a Pi expression
become required arguments in the lambda function. The type we are
paramaterizing the function with must be supplied in the
argument. This is because there exists an identity function for every
single type `E`. Here is how you would use this identity function.

~~~ {.lisp .html}
> (id Nat 10)

(the Nat 10)

> (id (Pair Nat Atom) (cons 10 'tacos))

(the (Pair Nat Atom)
  (cons 10 'tacos))
~~~


## Creating the list-replicate function

Here is the complete definition of `list-replicate`. Similar to the
last post we are using the `rec-Nat` function. We use the `rec-Nat`
function to call a constructor function for each of the values of `n`
counting down to `zero`. We know at `zero` that our base-case is the
empty list of type E, `nil`. The constructor for lists is `::`. It
isn't called `cons` in Pie because `cons` is for creating `Pair`s.

Our step function is a bit more complicated, however. It must accept
the type `E` as an argument so that `list-step-replicate` and
`list-replicate` are talking about the same exact type E. The second
argument is the element of type `E` which we want replicated.

~~~ {.lisp .html}

(claim list-step-replicate
  (Pi ((E U))
    (-> E Nat (List E)
        (List E))))

(define list-step-replicate
  (λ (E e n list)
    (:: e list)))

(claim list-replicate
  (Pi ((E U))
    (-> Nat E
      (List E))))

(define list-replicate
  (λ (E n e)
    (rec-Nat n
      (the (List E) nil)
      (list-step-replicate E e))))
~~~

## Review

* In this post we demonstrated how to replicate an element `n` number
  of times.
* We learned about Pi expressions for declaring types.
