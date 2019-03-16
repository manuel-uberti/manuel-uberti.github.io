---
layout:     post
title:      Switching buffers (Take 2)
date:       2019-03-16
summary:    A custom function to switch buffers.
categories: emacs
---

[Last month](https://manuel-uberti.github.io/emacs/2019/02/05/nswbuff/) I wrote
about the neat [nswbuff](https://github.com/joostkremers/nswbuff), but there is
another way to implement buffer switching without introducing a new package.

Since I already use
[counsel-projectile](https://github.com/ericdanan/counsel-projectile), why not
leverage it to my needs?

``` emacs-lisp
(defun mu-switch-to-project-buffer-if-in-project (arg)
    "Custom switch to buffer.
With universal argument ARG or when not in project, rely on
`ivy-switch-buffer'.
Otherwise, use `counsel-projectile-switch-to-buffer'."
    (interactive "P")
    (if (or arg
            (not (projectile-project-p)))
        (ivy-switch-buffer)
      (counsel-projectile-switch-to-buffer)))

(bind-key* "C-x b" #'mu-switch-to-project-buffer-if-in-project)
```

Pretty self-explanatory. By default, when not in a project
`counsel-projectile-switch-to-buffer` asks you for the project to switch to.

However, if I am not in a project chances are I want to switch to a buffer that
doesn’t belong to a project, especially since I usually enter a project before
switching to one of its buffers.

[nswbuff](https://github.com/joostkremers/nswbuff) has previews and
back-and-forth navigation, so it still offers a nicer solution to buffer
switching. This is Emacs, of course, so you know the deal: endless
possibilities.
