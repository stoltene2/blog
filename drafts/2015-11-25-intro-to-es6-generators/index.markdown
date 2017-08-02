---
title: Intro to ES6 generators
author: Eric Stolten
published: false
---

Generators are an interesting new feature in the ES6 specification. Below I'm
going to show you a few examples that demonstrate this new feature. First I am
going to demonstrate the syntax and build motivation piece by piece.

[Newer versions of Chrome support generators.]() Feel free to tag along in the
developer console.

### Generator function syntax

The syntax is super simple. Wherever you would normally use a `function`
declaration you can turn it into a generator with `function*`. No problem.

~~~ {.sourceCode .javascript}
function* exConsole() {
  console.log('My first generator')
}
~~~

But, after you create the object, then what? What can you do with it?

~~~ {.sourceCode .javascript}
> let a = exConsole();
undefined
~~~

Huh, didn't expect that. I expected to see console output. If I execute the
following function I'll get console output.

~~~ {.sourceCode .javascript}
function exConsoleNoGenerator() {
  console.log('No generator')
}
~~~

~~~ {.sourceCode .javascript}
> var b = exConsoleNoGenerator();
No generator
undefined
~~~

In the next section I will show you how to move the flow along.

### Generator function API

The API for generators is that of the Iterator interface. Mainly, because once
constructed, the generator returns an iterable object which has the iterable
interface, `next()` and `throw()`. Invoking `next()` begins running a generator
up until execution reaches a yield statement[^1].  `throw` provides a
mechanism for raising errors while working with generators.

[^1]: [MDN Documentation for `function*`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function*)

Here is that first example again.

~~~ {.sourceCode .javascript}
> let a = exConsole();
> a.next();
My first generator
Object {value: undefined, done: true}
~~~

The return object has two properties, `value` and `done`.  The first property
`value` is the result of the expression passed to `yield`[^2]. In the case
above, we have not used the `yield` keyword, and no expression has been
returned. The second property, `done`, indicates that the iterator and
consequently the generator has finished running.

[^2]: [MDN Documentation for `yield`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/yield)

### Iterating values

Now, I will show you an example of the yield keyword in action. Take a second
and try this function in your browser.

~~~ {.sourceCode .javascript}
function* exYield() {
  yield 1;
  yield 2;
}
~~~

What do you think happs with this function? Here is a sample of the output

~~~ {.sourceCode .javascript}
> a = exYield();
> a.next();
Object {value: 1, done: false}

> a.next();
Object {value: 2, done: false}

> a.next();
Object {value: undefined, done: true}

~~~

The initial call to `next()` _begins_ the execution of the generator _pausing_ at the
first `yield` statement. The following call to `next()` _resumes_ execution
while _pausing_ again at the next yield expression. Finally, execution of the
generator finishes.

To give you a better feel for how this flow works I inserted console statements
in between yield expressions.

~~~ {.sourceCode .javascript}
function* exYieldLog() {
  console.log('Preparing');
  yield 1;
  console.log('1 yielded');
  yield 2;
  console.log('2 yielded');
}

> a = exYieldLog();
> a.next();
Preparing
Object {value: 1, done: false}

> a.next();
1 yielded
Object {value: 2, done: false}

> a.next();
2 yielded
Object {value: undefined, done: true}
~~~

Now that we've gotten our feet wet. Lets see an example of iterating through a
list. These further examples are to show you that you can invoke yield from
inside any control structure within the generator.


~~~ {.sourceCode .javascript}

function* exList(lst) {
  let l = lst.length;
  for (let i=0; i<l; i++) {
    yield lst[i];
  }
}
~~~

Just as we expect, if we pass a list of `[1,2,3]` into the generator, we will be
able to iterate over the values.

~~~ {.sourceCode .javascript}
> a = exList([1,2,3])
exList {[[GeneratorStatus]]: "suspended", [[GeneratorReceiver]]: undefined}

> a.next()
Object {value: 1, done: false}

> a.next()
Object {value: 2, done: false}

> a.next()
Object {value: 3, done: false}

> a.next()
Object {value: undefined, done: true}
~~~

### Generating infinite streams

Here is where things get interesting.

~~~ {.sourceCode .javascript}

function* exRandom(n) {
  while(true) {
    yield Math.floor(Math.random() * n);
  }
}
~~~

Do you notice that we have a `while(true)` right in the center of that
function? This is intentional. This generator will never return and
will give us a new random number bound by `n`, forever.

~~~ {.sourceCode .javascript}
> a = exRandom(10);
> a.next()
Object {value: 7, done: false}

> a.next()
Object {value: 6, done: false}

> a.next()
Object {value: 2, done: false}
~~~

A fairly typical example of an infinite stream is the Fibonacci
generator. I think computing fibonacci generators is an overused
interview question. Interviewers wouldn't expect you to do this.

~~~ {.sourceCode .javascript}
function* exFib() {
  'use strict';
  let i = 1;
  let j = 1;
  let tmp = 0;

  while(true) {
    yield i;
    tmp = i;
    i = j;
    j = i + tmp;
  }
}
~~~

Here is the generator in action.

~~~ {.sourceCode .javascript}
> a = exFib()
> a.next()
Object {value: 1, done: false}

> a.next()
Object {value: 1, done: false}

> a.next()
Object {value: 2, done: false}

> a.next()
Object {value: 3, done: false}

> a.next()
Object {value: 5, done: false}
~~~

## Wrap up

Next time I will show you how to use generators in conjuction with
each other.

Follow me on [Twitter](https://twitter.com/ericstolten) and let me
know if you have any questions.

Until next time.
