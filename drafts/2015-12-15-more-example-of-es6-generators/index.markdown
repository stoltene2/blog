---
title: More examples of ES6 Generators
author: Eric Stolten
published: false
---

Last time... we covered
[Introduction to ES6 generators](../2015-11-25-intro-to-es6-generators/index.html)

### Influencing the flow of the generator

#### But...wat?

### Unwrapping promises into a more declarative flow

### Failure!?

### Debugging generators in Chrome

Here are more examples?

~~~ {.sourceCode .javascript}

function* exEven() {
  let r = 0;
  while(true) {
    r = yield r;

    if(r % 2 === 0) {
      console.log('even');
    }
  }
}

function* exPrint() {
  let v = 'Starting';
  while(true) {
    v = yield v;
    console.log(v);
  }
}

function* exEven2(c) {
  let r = 0;
  while(true) {
    r = yield r;
    if ( r % 2 === 0) {
      c.next(r);
    }
  }
}
~~~
