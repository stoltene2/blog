---
title: From Thinking Sphinx to Elastic Search
author: Eric Stolten
published: true
---

Sometimes systems change and we need to change with them.

Being a responsible developer we need to minimize the impact of our
change.  To best do this, sometimes we need to incrementally change
small portions of our systems until the migrations are complete.  It
feels like you spend more time on the planning stage than the
execution.  But there is no greater feeling then when the migration is
complete and you are left saying to yourself, "that was easy."

<!--more-->

If you've ever tried changing from Thinking Sphinx to Elastic Search
incrementally, you might run into some problems.  Here, I present a
rough set of instructions for moving to Elastic Search in your rails
project.

1. Find the same functionality between your search subsystems.
1. Choose the best plan of attack.
1. Write your tests and test against your existing models

	Construct tests in sphinx and assert that you get the same results
    out of Elastic Search This may take a while.
	
1. Use prefixes in your index names.
1. Make the index creation part of your deploy process

Add more helpful tips here...

During our migration we moved from Thinking Sphinx (Gem name) to the
excellent karmi/tire gem.  Both systems will attempt to add a #search
method to your model.  Normally, this would make the systems
incompatible.  If you read the tire source code, you will find that if
there is already a #search method on your model it will not add one
for you.  Great!  You can access the elastic search by calling through
your model to tire.search.

A good strategy is to not mix in the tire gem until after the thinking
sphinx has been loaded on your model.

I have some models that have basically the same minimal functionality
with pagination so I will extract some of the commonalities into a
Ruby module.  This technique will futureproof your project.  If you
changed it once, why would you need to ever change it again, right?


~~~ {.ruby .numberLines}
tire.search do

end
~~~
	
