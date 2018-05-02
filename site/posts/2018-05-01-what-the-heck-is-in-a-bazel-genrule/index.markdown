---
title: What the heck is $&lt; in a Bazel genrule?
description: What is $< all about in Bazel anyway?
author: Eric Stolten
published: true
---

While learning about Bazel and making progress on achieving my desired
build configuration I kept encountering `$<` in genrule command
attributes. Whenever the command would appear I mentally skipped over
it because it appeared to be a shell variable unknown to me. Being
curious, I did some digging on google under this assumption and didn't
find much. It's difficult to search for symbol expressions and find
meaningful results. Eventually I stumbled on some of the Bazel
documentation which explains it's usage.

When you construct a
[genrule](https://docs.bazel.build/versions/master/be/general.html#genrule)
target you see that if there is a singular source defined it can be
referenced via `$<` in the command. These special variables come from
the [Makefile rules
expansion](https://docs.bazel.build/versions/master/be/make-variables.html). Here
is an example of the rule in action when there is only one source file
and one output file.

~~~ {.python}
genrule(
    name = "copy-file",
    srcs = [
        "//some:file",
    ],
    outs = ["concatenated.txt"],
    cmd = "cp $< $@",
)
~~~

The above snippet is just shorthand for using `$(locations ...)`
expansion as below.

~~~ {.python}
# Same as above but using location expansion
genrule(
    name = "copy-file",
    srcs = [
        "//some:file",
    ],
    outs = ["concatenated.txt"],
    cmd = "cp $(location //some:file) $@",
)
~~~

If you have multiple sources listed in a genrule target you cannot
reference them using `$<`. Instead, they need to be referenced with the
longhand form so that the file is referenced unambiguously. The long
form is `$(locations //something/listed/in/srcs)` as below. Notice that
this is using the plural, locations, to find the file.

~~~ {.python}
genrule(
    name = "concat_all_files",
    srcs = [
        "//some:files",  # a filegroup with multiple files in it ==> $(locations)
        "//other:gen",   # a genrule with a single output ==> $(location)
    ],
    outs = ["concatenated.txt"],
    cmd = "cat $(locations //some:files) $(locations //other:gen) > $@",
)
~~~

Lastly, you'll see that a singular output is referenced as `$@`.

If you would like to see what other environment variables are
available you can run `$bazel info --show_make_env`.

Hopefully this is helpful for you.
