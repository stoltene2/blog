---
title: Racket Reading Group - Session 1 and Session 2
author: Eric Stolten
published: true
description: Summaries of what was covered in our first two sessions
---

This is a continuation of the Racket [reading group](../2018-01-16-2018-racket-reading-group/).

The first session got us off the ground. Everyone was able to buy the
book and cover the first 4.5 chapters.

Apart from covering logistics of the reading group like agreeing on a
pace. We covered some of the basics of Dr. Racket[^1]. We showed off a few
of the concepts that occurred in the first several chapters of the
book[^2]. The next chapter we agreed to read covers an introduction to the
[big-bang](http://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=big-bang#%28form._world._%28%28lib._2htdp%2Funiverse..rkt%29._big-bang%29%29)
macro.

[^1]: [Dr Racket](http://docs.racket-lang.org/drracket/index.html)
[^2]: [Realm of Racket](https://www.amazon.com/Realm-Racket-Learn-Program-Game/dp/1593274912)

We finished up the meeting by showing off a pokemon themed missile
defense game made by someone in the group during a college class. It
gave us an idea of the types of games we could create with
Racket. Very cool stuff.

Here are some things that people liked from this session.

### Debugging with Dr. Racket

Dr. Racket has some great features for new developers to track errors
in their code. When the Racket VM crashes with an error we are
presented with a great visual display pointing to exactly where the
error occurred.

I personally like this feature a lot. Even as an avid user of Emacs I
appreciate when something is done well. I think VSCode, Webstorm,
Visual Studio, etc. have enough information available to them through
stack traces for rendering helpful feedback overlays.

Now, I feel obligated to mention that errors in Emacs are difficult to
debug. For some reason the stack traces are un-helpful and cryptic
with pointing out errors. It isn't easy for me to determine if this is
because of Geiser mode or if Dr. Racket is doing something
automagically behind the scenes.

Pointing out code identifiers is cool in Dr. Racket as well. This
helps beginners determine where identifiers come from.

![Identifiers are cool](images/racket-references.png)

### If expressions

We talked a bit about the difference between expressions and
statements. I offered some insight into how nice expressions are to
work with especially in a language like Haskell. Some people found it
odd that you always need to provide an else value.

In the example below, the variable `if-result` will be assigned the
value of 1.

~~~.racket
(define if-result (if #t 1 2))
~~~

### Macros

We spent a few minutes talking about macros and how they generate code
for us. A prime example for us is the `struct` macro. Once the
semantics were understood it feels less magical from where certain
accessor functions come from. We pointed out that `structs` are a little
cumbersome to use with what we know about them from our brief
introduction. Looking ahead in the material we see there are solutions
to these problems but we haven't gotten there yet.

Renaming the field members of a struct can be cumbersome. For this
task we found it helpful to use Dr. Racket.

### Session 2

Our goal for session 2 was to cover chapter 5 and chapter 6. These two
chapters cover an introduction to the big-bang macro with a small
landing space craft and a snake game.

We didn't quite cover all of the two chapters. We noticed that the
snake game had pieces we needed to define ourselves. We decided that
the group should spend another week working through the snake game and
making our own custom tweaks to any game of our choosing.
