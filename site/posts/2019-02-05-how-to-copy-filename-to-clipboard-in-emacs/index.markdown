---
title: How to copy filename to clipboard in Emacs
author: Eric Stolten
description: Here is a small snippet of elisp to copy the buffer's filename to the system clipboard.
---

I often find myself wanting to get the path of a file I'm visiting to
reference it with my team. Here is a quick snippet of Elisp to copy a
buffer's filename, if it exists, to the system clipboard.

When this function is run it gets the filename of a buffer if it
exists, copies the name into a temporary buffer and adds it to the
system clipboard. This is done by using the `copy-region-as-kill`
function. If you know of a shorter way, please tweet at me.

~~~ {.lisp .html}
(defun es/copy-buffer-file-name-to-clipboard ()
  "Copies the buffer file name to the clipboard"
  (let ((buf-name (buffer-file-name)))
    (if buf-name
        (with-temp-buffer
          (insert buf-name)
          (copy-region-as-kill (point-min) (point-max))
          (message "Copied %s to clipboard" buf-name))
      (message "Your buffer is not backed by a file"))))
~~~

You can view my [Emacs configuration on GitHub](https://github.com/stoltene2/emacs-config).
