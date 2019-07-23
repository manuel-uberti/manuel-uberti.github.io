---
layout:     post
title:      Keeping your .emacs.d clean
date:       2017-06-17
summary:    House-cleaning with `no-littering`.
categories: emacs
---

How often do you have a look at your `.emacs.d` directory? If you’re anything
like me, not that often. It’s no wonder then that upon opening it I was
horrified to see it cluttered with all sorts of files from all the packages
I regularly use.

Jonas Bernoulli, of [Magit](https://magit.vc/) fame, must be feeling similar
concerns. Otherwise, why did he come up with a package
like [no-littering](https://github.com/tarsius/no-littering)?

Installation is trivial, especially if you use the great `use-package`:

``` emacs-lisp
(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))
```

No extra setup is required. Place this snippet somewhere at the beginning of
your `init.el` file and you are set. Use Emacs for a while and then go back
checking `.emacs.d`: nice, huh?

To be honest, I wiped out my `.emacs.d` directory and started from fresh with
`no-littering` installed. Too much, maybe, but I was eager to see how good this
package is. And yes, it is good.

House-cleaning has never been so easy.
