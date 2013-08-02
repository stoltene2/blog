---
title: Why I Chose Hakyll for my Blog
author: Eric Stolten
---

[Hakyll](http://jaspervdj.be/hakyll/) is a great system that
allows you to write markdown blog pages that will be transformed into html pages
after applying custom style and templates. I am currently using version *4.3.1.0*.

The flexibility of the system is great especially if you have
experience hacking with Haskell.  Even without experience you can find
a fair number of people's blogs with their source code online for
inspiration.

I like many of the advantages of running a static blog site.
Primarily, it is the least resource intensive option for serving a web
site.  Also, you don't need to worry about server restarts.  I choose
to use [Hakyll](http://jaspervdj.be/hakyll/) because I like to support
the community.

There are a few changes that I plan on making with my blog.

1. Remove the .html extension from all pages and let Nginx know that
   we mean to serve html pages.

By removing the .html extension we can futureproof our site and make
the urls more uniform for my taste.  If in the future we want to
change the blog to a more dynamic blog, we can do so without changing
any of the links or paths on the site.

