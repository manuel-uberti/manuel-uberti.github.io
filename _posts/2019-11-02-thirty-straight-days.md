---
layout:     post
title:      Thirty straight days 
date:       2019-11-02
summary:    A month with `straight.el`.
categories: emacs
---

It’s already been a month since I moved from `package.el` to
[straight.el](https://github.com/raxod502/straight.el), and if it wasn’t for the
constant fiddling with my Emacs configuration, I would have hardly noticed the
differences in package management. A proof of the solid work behind
`straight.el`, sure, but also a demonstration of how painless package management
can be for someone who bypasses the provided UI and relies on
[use-package](https://github.com/jwiegley/use-package) snippets to install and
setup their packages.

However, with `straight.el` a new approach to updates is needed. First of all,
I compulsively track the state of the packages I have installed, and I tend to
update all of them every morning. This is an operation that follows the pulling
of the latest developments on the Emacs `master` branch.

On the one hand, this approach could lead to breaking changes and tears of
sadness. On the other hand, I like to report bugs as soon as I discover them,
and hopefully help the great developers out there who make my text editing a
wonder.

Considering how vital to my computing Emacs is, this may sound like complete
madness, I know. In my defence, I mostly rely on stable and long-term support
software on my computer. Emacs is just the special kind of madness that drags me
to it in spite of the risks. Furthermore, as I wrote [last
time](https://manuel-uberti.github.io/emacs/2019/10/04/straight/), `straight.el`
makes it easier to revert to a previous unbroken version of a package, so I can
find a functional environment without too much of a hassle.

In my `package.el` days, package updating was handled by
[Paradox](https://github.com/Malabarba/paradox). In a matter of a couple of key
bindings, it took care of notifying me about new versions and installing the
selected updates. `straight.el` is less fancy, though. It provides the necessary
commands to install and update a package, as well as the big brother that will
pull every package in, but all the UI it offers is the
user’s [completing-read](http://doc.endlessparentheses.com/Fun/completing-read.html)
framework—`ivy-read` in my case.

Well, like the first line of the README says, `straight.el` is a tool for the
Emacs hacker, so let’s play with it. All I had to do to get a more *interactive*
user experience when updating packages was devising a small wrapper around
`straight.el`’s facilities.

``` emacs-lisp
(defun mu-straight-pull-or-prune (&optional prune)
  "Update all available packages via `straight'.
With PRUNE, prune the build cache and the build directory."
  (interactive "P")
  (if prune
      (when (y-or-n-p "Prune build cache and build directory?")
        (straight-prune-build-cache)
        (straight-prune-build-directory))
    (when (y-or-n-p "Update all available packages?")
      (straight-pull-all))))

(bind-key* "<f7>" #'mu-straight-pull-or-prune)
```

Pretty straightforward, isn’t it? (Pun intended.)
