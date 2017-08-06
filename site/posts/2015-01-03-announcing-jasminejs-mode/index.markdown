---
title: Announcing jasminejs-mode 1.0
description: jasmine-js mode adds keyboard shortcuts and snippets for common testing workflows.
author: Eric Stolten
---

### jasminejs-mode for Emacs ###

#### Motivation ####

I created this mode out of necessity. While writing
[Jasmine](http://jasmine.github.io) tests I found myself performing
the same actions over and over again. Specifically, inside a large
test suite I would focus a single test or group of tests to run using
the `iit` and `ddescribe` conventions.

#### Usage ####

With the default prefix keys, `C-c C-j`, you can use the four major commands.

* `it` -- *I*t *T*oggle between `it` and `iit`
* `ip` -- *I*t *P*ending between `it` and `xit`
* `dt` -- *D*escribe *T*oggle between `describe` and `ddescribe`
* `dp` -- *D*escribe *P*ending between `describe` and `xdescribe`

My intention was to create keybindings with symmetry. Notice that the
prefix key is `C-c C-j`. To me this is my mental reminder that I mean
jasmine mode. Next, each comman either targets an `it` or `describe`
expression. With each command you can focus or set the expression to
pending.

#### Installing ####

See the [GitHub Repo](https://github.com/stoltene2/jasminejs-mode) for
instructions on how to configure it. You can install it through
[Melpa](http://melpa.org/#/jasminejs-mode) for convenience.

#### Other Capabilities ####

Included with this mode are some useful yasnippets. See the
[GitHub Repo](https://github.com/stoltene2/jasminejs-mode) for
instructions on configuring the snippets. As with the key-bindings I
created snippets with aliases that are intuitive as possible. Here are
some helpful snippets.

| key       | description                           |
|--------|-------------------------------------|
| `afte`     | `afterEach(...)`                        |
| `befe`     | `beforeEach(...)`                       |
| `desc`     | `describe(...)`                         |
| `ex`       | `expect(...)`                           |
| `expb`     | `expect(...).toBe(...)`                 |
| `expc`     | `expect(...).toHaveBeenCalled()`        |
| `expcw`    | `expect(...).toHaveBeenCalledWith(...)` |
| `expe`     | `expect(...).toEqual(...)`              |
| `expf`     | `expect(...).toBeFalsy()`               |
| `expt`     | `expect(...).toBeTruthy()`              |
| `it`       | `it(...)`                               |

#### Contributing ####

Head on over to
[GitHub Repo](https://github.com/stoltene2/jasminejs-mode) and have a
look. I'm interested in knowing what features may be useful to you. If
you think of something, open an issue and we can discuss. Also, if you
like this mode, send me a message and let me know.

#### Future Versions ####

There are some changes coming to [Jasmine](http://jasmine.github.io) in version 2.1 where `fit` and `fdescribe` are the standard way to focus tests. If there is a desire, I will implement this feature.
