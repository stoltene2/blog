---
title: Prefer directives to ng-include
author: Eric Stolten
published: true
---

_Did you push Angular to it's limits?_

I thought so. But something didn't feel right.

In this article I will show you how to speed up your AngularJS
code. There may be code in your AngularJS application that can cause
perplexing performance problems.

I experienced some of these problems first hand and overcame one of
them by with the method described below. In this specific scenario the
culprits were an abuse of `ngInclude` directives nested inside
`ngRepeat` directives. I will show you how to increase performance in
your application if you use these two directives together.

I hope you improve your performance like I did.

## See the problem in action

<div class="alert alert-info" role="alert">
<p>

Follow along by cloning the sample app from the [Github Repo](https://github.com/stoltene2/blog-ng-include-perf-problems).
</p>
</div>

##### Goals

* Kept the data as simple as possible
* Added basic style to make the DOM larger and look somewhat clean
* Small amount of code to demonstrate
* Use a lot of elements to see the problem
* Templates are _split up_ into multiple files for readability

To keep the sample application simple for demonstration purposes
I attempted to keep the view simple. In a real application you
would likely have **more complicated** markup and you could see this
problem with _fewer_ repeated nodes.

The layout of the application is quite simple as you can see
below. There is one main controller that generates a list of
products. You can change the number of products be typing the desired
number into the form and clicking _Generate_. If you click on the
title of any product you will expand all products in the list.

The expansion of all the products is the task that will take quite a
bit of time in the **master** branch of the repository.

You can see below that I created a template structure that matches the
data as close as possible.

<div class="row">
<div class="col-xs-12 col-sm-6">
![](images/template-layout.png)
</div>

<div class="col-xs-12 col-sm-6">

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

### Template snippets
Here are some snippets of the template structure for more context.

##### product.html
~~~ {.html}
<div class="row bs-callout">
    <ng-include src="'views/product-title.html'"></ng-include>
    <ng-include src="'views/product-details.html'"></ng-include>
</div>
~~~

##### product-title.html
~~~ {.html}
<div class="row">
    <h3 class="col-sm-6 title" ng-click="expand(product)">{{::product.name}}</h3>
</div>
~~~

##### product-details.html
~~~ {.html}
<div ng-if="product.expanded" class="row">

    <div class="row description">
        <div class="col-sm-8">
            <p>{{::product.description}}</p>
        </div>
        <div class="col-sm-4">
            <button type="button" class="btn btn-primary">Buy Now</button> ${{::product.price}}
        </div>

    </div>

    <div class="col-sm-12">
        <ng-include src="'views/product-specs.html'"></ng-include>
    </div>

    <div class="col-sm-12">
        <ng-include src="'views/product-reviews.html'"></ng-include>
    </div>
</div>
~~~

##### product-specs.html
~~~ {.html}
<div class="row">
    <h4>Specifications</h4>
    <div class="col-sm-12" ng-repeat="spec in ::product.specifications">
        <div class="row">
            <div class="col-sm-4">{{::spec.name}}</div>
            <div class="col-sm-4">{{::spec.value}}</div>
        </div>
    </div>
</div>
~~~

##### product-reviews.html
~~~ {.html}
<div class="row">
    <h4>Reviews</h4>
    <div class="description" ng-repeat="review in ::product.reviews">
        <div class="row">
            <div class="col-sm-8"><strong>Author</strong>: {{::review.userName}}</div>
            <div class="col-sm-4"><strong>Rating</strong>: {{::review.rating}}</div>
        </div>
        <div class="row review-body">
            <p class="col-sm-8">{{::review.review}}</p>
        </div>
    </div>
</div>
~~~

When you look at these templates, one thing that should stand out is
the judicious use of `ngInclude`. This was used in a situation where
the `product.html` template became much too large to manage
changes.

The desire to split templates into smaller templates has many
benefits. Besides having a smaller context the biggest win appears in
the reduction of merge conflicts. Bad merges lead to
great frustration.

In a similar scenario you may follow this path instead of using
alternative methods. See what happens below if you do.

### The problem

<div class="alert alert-info" role="alert">
<p>
Still following along? Start the application with `grunt serve`.
</p>
</div>

Play around with the application. Generate 1000 products in Chrome then click on a headline.

Did you notice how long that took?

It is only partly because there are 1000 elements. I've included some
screen shots of the [Chrome time-line](https://developer.chrome.com/devtools/docs/timeline) tool below.

<br/>
<div class="row">
<div class="col-sm-6">

![](images/expand-all-ng-include.png)

</div>
<div class="col-sm-6">

What stands out here?

* **7.68s**!? Why did this take so long?
* Javascript seems busy
* What are those blue regions with yellow mixed in?

</div>
</div>

<br/>

<div class="row">
<div class="col-sm-6">

![](images/expand-all-ng-include-zoom-1.png)
</div>
<div class="col-sm-6">

Interesting. These are not solid blue regions but specific items that
are executing **repeatedly**. We also see that the intermixed yellow
regions are garbage collection. Let's zoom in another level to see
what those blue regions more clearly.

</div>
</div>

<br/>

<div class="row">
<div class="col-sm-6">

![](images/expand-all-ng-include-zoom-2.png)
</div>
<div class="col-sm-6">

It looks like we are doing **a lot** of parsing. This makes sense if we
think about it. Given the design of the application we include a new
template in the DOM for each _Product_ when we expand all
details.

The template layout feels clean, but something is wrong.

Taking a step back we should ask, "shouldn't those templates be compiled once for
efficiency?" The answer is yes, but how do we get there? The simple
answer is to use what I've called _Thin Directives_.

</div>
</div>

## Thin Directives

Use _Thin Directives_.

What is a _Thin Directive_? A _Thin Directive_ is a directive whose
sole purpose is to provide a template. Here is the refactoring of the
`ngInclude`s that we had in the beginning of the post.

<br/>

~~~ {.javascript}
angular.module('app').directive('productTitle', function() {
  return {
    templateUrl: 'views/product-title.html'
  };
});

angular.module('app').directive('productDetails', function() {
  return {
    templateUrl: 'views/product-details.html'
  };
});

angular.module('app').directive('productReviews', function() {
  return {
    templateUrl: 'views/product-reviews.html'
  };
});

angular.module('app').directive('productSpecs', function() {
  return {
    templateUrl: 'views/product-specs.html'
  };
});
~~~

<br/>

I've replaced **all** `ngInclude` directives with the new custom
directives. To save space I'll only include one example.

<br/>

##### product.html
~~~ {.html}
<div class="row bs-callout">
    <product-title></product-title>
    <product-details></product-details>
</div>
~~~

#### Lets check our example

<div class="alert alert-info" role="alert">
<p>
Check out the [thin-directives](https://github.com/stoltene2/blog-ng-include-perf-problems/tree/thin-directives) branch. Re-launch!
</p>
</div>

Have a look at the performance differences below. We've nearly **halved** the performance with 3.27s!

<br/>
<div class="row">
<div class="col-sm-6">
![](images/expand-all-components.png)
</div>
<div class="col-sm-6">
![](images/expand-all-components-zoom.png)
</div>
</div>


## Why does this work?

Directives only run compile once by default unless you provide extra
compile functions.

`ngInclude` is a bit more complicated. It has a watcher on the `src`
attribute inside the `compile` function. Have a look at the
[ngInclude source](https://github.com/angular/angular.js/blob/master/src/ng/directive/ngInclude.js#L218) for some more insight.

## Summary

**Is ngInclude bad?**

No. You need to pay special attention to where and how you use
it. It's problem is exacerbated because it is nested in multiple
`ngRepeat` elements. If used sparingly you would likely never notice
the difference.

However, I recommend creating **more** components from the
beginning. It produces more source files but you will benefit _greatly_ from
the granular architecture of components. Besides most web frameworks
are heading this way too.

It was
[Ben Nadel's blog post that inspired my effort](http://www.bennadel.com/blog/2738-using-ngrepeat-with-nginclude-hurts-performance-in-angularjs.htm)
to find some extreme performance problems in my application.

Thank you, Torgeir, over at
[Syntax success](http://www.syntaxsuccess.com) for testing the nuances
of Angular.

Let me know on Twitter if this was helpful to you!
