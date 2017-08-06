---
title: Update of jasminejs-mode 2.0
author: Eric Stolten
description: This mode has been changed to accomodate Jasmine 2.1 and beyond for test focusing.
date: 2015-05-29
---

### Breaking change to jasminejs-mode ###

This mode has been changed to accomodate Jasmine 2.1 and beyond for test focusing.

#### Changes ####

With the default prefix keys, `C-c C-j`, you can use the four major
commands. `it` and `dt` were changed to use `fit` and `fdescribe` for
focusing **instead** of `iit` and `ddescribe`.

* `it` -- *I*t *T*oggle between `it` and `fit`
* `ip` -- *I*t *P*ending between `it` and `xit`
* `dt` -- *D*escribe *T*oggle between `describe` and `fdescribe`
* `dp` -- *D*escribe *P*ending between `describe` and `xdescribe`

#### Installing ####

See the [GitHub Repo](https://github.com/stoltene2/jasminejs-mode) for
instructions on how to configure it. You can install it through
[Melpa](http://melpa.org/#/jasminejs-mode) for convenience.

#### Contributing ####

Head on over to
[GitHub Repo](https://github.com/stoltene2/jasminejs-mode) and have a
look. I'm interested in knowing what features may be useful to you. If
you think of something, open an issue and we can discuss. Also, if you
like this mode, send me a message and let me know.
