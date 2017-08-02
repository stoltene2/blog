---
title: Find that element in Chrome
author: Eric Stolten
published: true
---

First, open up the developer tools in _Chrome_. With the developer tools open,

(@) Click the magnifying glass.

    ![](/images/magnifying-glass.png)

(@) Select the element on the page by **clicking**.

    ![](/images/select-region.png)

    <br/>

    Notice the highlighted element.

    <br/>
    ![](/images/selected-element.png)


(@) Navigate to your console

(@) Assuming your page has jQuery style selectors, select your element
with `var el = $($0);`. The currently selected _DOM_ node is
referenced by `$0`.

If you are using a front-end framework like
[AngularJS](http://www.angularjs.org) then you might be interested in
finding the scope for a node and examining its attributes.

(@) Example

    ![](/images/selected-element-console.png)
    

#### Web console snippet (try me out)
```javascript
var el = $($0);
var scope = $($0).scope();
scope.$$watchers.length
```





