---
layout:     post
title:      Helmify some completion mechanisms
date:       2020-01-01
summary:    Using `helm-dabbrev` and `helm-company`.
categories: emacs
---

Last year I wrote about [making Company completion popup appear on
demand](https://manuel-uberti.github.io/emacs/2019/01/17/company-on-demand/).
There I also briefly mentioned `dabbrev-expand` and `dabbrev-completion`, but
I have never mentioned `hippie-expand`, and that it has been pretty much
covering my completion needs.

However, [moving to
Helm](https://manuel-uberti.github.io/emacs/2019/11/16/helm/) prompted me to
give `helm-dabbrev` a try. `helm-dabbrev` works similarly to
`dabbrev-completion`, but when you hit `helm-dabbrev-cycle-threshold` (which
defaults to 5), it takes you to a Helm buffer with all the benefits that come
with it.

Two settings are necessary to have `helm-dabbrev` behave like I want to. First,
I turned off Helm’s show completion, a feature which displays candidates in the
current buffer. I don’t like popups, as you may have noticed, and I prefer Helm
to always show candidates in the same position of the screen.

``` emacs-lisp
(setq helm-turn-on-show-completion nil)
```

Then, I made `helm-dabbrev` look for completions in all the available buffers.

``` emacs-lisp
(setq helm-dabbrev-related-buffer-fn nil)
```

By default, `helm-dabbrev` scans buffers with the same major mode as the current
one, but this doesn’t help when, for instance, I am writing a commit message and
I want a symbol to be completed. Binding `helm-dabbrev` to <kbd>C-.</kbd> is all
that’s left to do.

Moreover, Helm improves on Company too. With the help of
[helm-company](https://github.com/Sodel-the-Vociferous/helm-company)[^1] is
possible to narrow down the candidates through Helm smart UI. <kbd>C-TAB</kbd>
is my go-to key binding for `helm-company`.

Since Helm has become a great companion during my Emacs sessions, it makes sense
to rely on it for everything concerning completion.

## Footnotes

[^1]: I used to maintain `helm-company` before discovering Ivy, and now I’m back
    to it. The Emacs world is full of surprises.
