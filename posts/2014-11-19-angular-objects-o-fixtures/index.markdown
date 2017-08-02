---
title: Angular objects o'fixtures
author: Eric Stolten
published: true
---

In an [AngularJS](http://www.angularjs.org) project at work, I found
myself dealing with large JSON fixtures in my tests. You write tests,
right? right!? I wanted something that would require less module
includes and allow me to group fixtures into one object.

### Announcing
#### karma-fixture-to-angular-module-preprocessor

Check the project out on
[npm](https://www.npmjs.org/package/karma-fixture-to-angular-module-preprocessor)
or
[Github](https://github.com/stoltene2/karma-fixture-to-angular-module-preprocessor).
Comments & pull requests are welcome.

### Use it

After you have configured the
[plugin](https://github.com/stoltene2/karma-fixture-to-angular-module-preprocessor#configuration),
you can take a directory of JSON files and and return them in an
object of your choosing.

~~~~shell
$ tree
├── placeholder.js
├── AuthFailure.json
├── AuthSuccess.json
└── LoginResponse.json
~~~~~~~~~~~~~~~~~~~~~~

With an example configuration like this:

~~~~javascript
jsonDirToJs: {
  'path/to/placeholder.js': {
    moduleName: 'my.test.fixtures.auth',
    dir: 'path/to',
    objectName: 'MockAuth'
  }
}
~~~~~~~~~~~~~~~~~~~~~~

Now you can easily access your fixtures.

~~~~javascript
var MockAuth,
    login,
    authSuccess,
    authFail;

beforeEach(function () {
  module('my.test.fixtures.auth');
});

inject(function($injector){
  MockAuth = $injector.get('MockAuth');
  login = MockAuth.LoginResponse;
  authSuccess = MockAuth.AuthSuccess;
  authFailure = MockAuth.AuthFailure;
});
~~~~~~~~~~~

<br>

Happy coding.
