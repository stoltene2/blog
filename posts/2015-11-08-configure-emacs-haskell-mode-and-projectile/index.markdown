---
title: Configure Emacs haskell-mode with projectile
author: Eric Stolten
published: true
---

[Projectile](http://batsov.com/projectile/) is a powerful emacs mode
[especially when coupled with helm](http://tuhdo.github.io/helm-projectile.html). One
important feature for anyone writing tests is the ability to toggle
between a source file and its test file. This is accomplished in
projectile by invoking the interactive function,
`projectile-toggle-between-implementation-and-test` or the default key
binding of `C-c p t`. Unfortunately, this may not work right out of
the box for you depending on the test suffix used in your project.

If you are using stack, which is a great tool for Haskell development,
then the default configuration for projectile will need tuning. Check
out [Stack](https://github.com/commercialhaskell/stack) if you haven't
already. It might change the way you develop Haskell applications.

Below I will present two different options for configuring Haskell and
[Projectile](http://batsov.com/projectile/). First, global editor
configuration for your project settings. Second, project specific
settings for _each_ project. There are pros and cons to each approach.

## 1st Approach, global settings

The first approach, with global settings, is useful where
customizations don't change often.

Like your build command, test command, and test suffix, for
example. There are disadvantages to this approach if you work on
several projects of the same type with different conventions.

If you happen to work in a project that uses _cabal_ while another
project uses _stack_ then this is less desirable and even more severe
if you have a project using multiple languages or frameworks.

~~~ {.sourceCode .lisp}
(defun es/projectile-test-suffix (project-type)
  "Return test files of Spec for haskell-cabal projects
Use -spec for all other project types"
  (if (eq project-type 'haskell-cabal)
      "Spec"
    "-spec"))

(custom-set-variables
 '(projectile-test-files-suffices
   '("_test" "_spec" "Spec" "Test" "-test" "-spec"))
 '(projectile-test-suffix-function #'es/projectile-test-suffix)
 '(projectile-haskell-cabal-test-cmd
   (concat haskell-process-path-stack " test"))
 '(projectile-haskell-cabal-compile-cmd
   (concat haskell-process-path-stack " build")))
~~~

## 2nd Approach, .dir-locals.el

The project specific settings, is powerful but has consequences. In
this approach we utilize a special file named
[.dir-locals.el](http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables)
which allows you to specify project specific settings.

In our case, the test suffix function, the build command, and test
command. The power of
[.dir-locals.el](http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables)
comes from the ability to specify configuration based on current mode,
current directory, or sub-directory. This is powerful in a web
application where you have a test command for the backend and a
separate test command for Javascript tests.

The _biggest_ disadvantage is safety. If you check your
[.dir-locals.el](http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables)
file into source control, then Emacs will **automatically** attempt to
load these variables while opening each and every buffer. This will
result in annoying prompting and potentially the execution of
arbitrary code if you are not careful.

Now, you can get around this by declaring variables as safe in your
Emacs configuration as I will show you below. This essentially adds
commands verified by you to a whitelist. For the annoyance factor
alone, I would recommend not checking this file into source control
and provide friendly instructions on your Wiki for your fellow
Emacsers.

Do not create a situation where your project will attempt to execute
custom settings for fellow developers.

Here are the contents of a `.dir-locals.el` in the base of my project.

~~~ {.sourceCode .lisp}
((nil . ((projectile-test-suffix-function .
           (lambda (project-type) "" "Spec"))
         (eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            (concat haskell-process-path-stack " build")
                            projectile-compilation-cmd-map)

                   (puthash (projectile-project-root)
                            (concat haskell-process-path-stack " test")
                            projectile-test-cmd-map))))))
~~~

[Directory Local Variables](http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables)
have a special form. The outermost nil basically says to apply these
configurations in every circumstance. Instead, you can supply a mode
or even a sub-directory.

To disable prompting of
[safe file variables](http://www.gnu.org/software/emacs/manual/html_node/emacs/Safe-File-Variables.html#Safe-File-Variables)
you'll need to declare these variables as safe. If you follow this
approach, you should use this mechanism to white list the
configuration for your project.

Here is an example of whitelisting the settings from the dir-locals in
your local Emacs configuration.

~~~ {.sourceCode .lisp}
(custom-set-variables
 '(safe-local-variable-values
   (quote
    ((projectile-test-suffix-function lambda
                                      (project-type)
                                      "" "Spec")
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            (concat haskell-process-path-stack " build")
            projectile-compilation-cmd-map)
           (puthash
            (projectile-project-root)
            (concat haskell-process-path-stack " test")
            projectile-test-cmd-map))))))

~~~

Happy Emacs hacking. If you have any questions, please follow me on
Twitter. Let me know how this works for you.
