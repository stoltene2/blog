---
title: Prefer directives to ng-include
author: Eric Stolten
published: true
---

<div class="alert alert-warning" role="alert">
<p>
<b>Warning</b>: This article is a work in progress.
</p>
<p>
  Some details may be incorrect or incomplete.
</p>
</div>

# Random catch phrases
* Angular is slow should I switch to react?
* Angular is slow
      * Anything is slow when you do it wrong

# Versions I tried this on

| Version |
|---------|
| 1.3.15  |
| 1.4? |

Download the [source](http://www.github.com) and follow along.

In each section of this article may reference a different branch for
changes.

## Example app

1. Kept the data as simple as possible
1. Added some style to show the effect of dom parsing
1. The important of performance -- The browser needs to do more

<div class="row">

<div class="col-xs-12 col-sm-6">

#### Goal of the sample app

1. Loads a JSON object for a list of products
1. Each product contains
     * name
     * description
     * List of specifications
     * reviews
1. Each product hides the specifications and reviews by default to
   keep the DOM smaller. We will expand as necessary.
1. We expand the details only when we want to see more

Because we are creating a single page application we would like to
make as much data accessible to the application as possible.

</div>

<div class="col-xs-12 col-sm-6">

#### Sample JS Obj

~~~ {.javascript .html}
{
  products: [
    {
      name: "Develop fast apps",
      price: 10.35,
      description: "A long description",
      specifications: [
        {
          name: "weight",
          value: "3"
        }
      ],
      reviews: [
        {
          userName: "Eric Stolten"
          rating: 5,
          review: "Really long review"
        }
      ]
    }
  ]
}

~~~

</div>
</div>

## A natural way of things

1. Start with a large template with `ngRepeat`
1. Refactor it for readability

How one naturally chooses ng-include: large large templates
There isn't any deep reason to have directives

#### Make your code manageable
What happens when we have large repeated elements?
We break it down to smaller pieces. More files. Less confusion.
The world is rosy.

## A danger lurks

So far so good. Right? Well, as requirements change we find that we need to have *FIFTY*

* Add 1000!

### Timeline of expanding all

<div class="row">
<div class="col-sm-6">

![](images/expand-all-ng-include.png)

</div>
<div class="col-sm-6">

There are a few things to observe in this screenshot.

1. **7.68s**!? Why did this take so long?
1. Javascript seems busy
1. What are those blue regions with yellow mixed in?

I'll zoom in to see more.

</div>
</div>

### Zoom in on timeline

<div class="row">
<div class="col-sm-6">

![](images/expand-all-ng-include-zoom-1.png)
</div>
<div class="col-sm-6">

Interesting. Those are not solid blue regions but specific items that
trigger periodically. We can now see that the intermixed yellow
regions are garbage collection. Let's zoom in another level to see
what those blue regions more clearly.

</div>
</div>

### Zoom in a little more...

<div class="row">
<div class="col-sm-6">

![](images/expand-all-ng-include-zoom-2.png)
</div>
<div class="col-sm-6">

It looks like we are doing a lot of parsing. This makes sense if we
think about it. Given the design of the application we include a new
template in the DOM for each _Product_ when we expand their
details.

The templates are clean and separated. We can locate the components of
the `Product` by navigating to the named view. This feels clean, but
something is wrong.

Taking a step back we should ask, "shouldn't those templates be compiled once for
efficiency?" The answer is yes, but how do we get there? The simple
answer is to use what I've called _Thin Directives_.

</div>
</div>

## Solution: Thin Directives

What is a _Thin Directive_? A _Thin Directive_ is a directive whose sole purpose is to provide a template.

But why? _Explanation on how directives compile templates_

## Lets fixup our example

Show results

## Measure again
Wow that looks better.

<div class="row">
<div class="col-sm-6">
![devtools stuff](images/expand-all-components.png)
</div>
<div class="col-sm-6">
![devtools stuff](images/expand-all-components-zoom.png)
</div>
</div>

## What happens here?

Directives only run compile once if the template is a string.

`ngInclude` is not a string

Show a small example of compile running one vs. multiple times.

## Influences

* Tor
* That one blog article
* Experimenting

* Include links to using dev tools debugger
* Fix padding on paragraphs next to pictures to make them line up closer.
* Put pictures in a frame
