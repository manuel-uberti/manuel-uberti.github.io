---
layout:     post
title:      Taming closing delimiters in my s-expressions
date:       2017-09-17
summary:    How `syntactic-close` can simplify Lisp editing.
categories: emacs
---

As I explained when I wrote about my [daily Clojure
workflow](https://manuel-uberti.github.io/programming/2017/04/29/clojureenv/),
I rely heavily on [Smartparens](https://github.com/Fuco1/smartparens) for my
editing. With Lisp-like languages in particular, I enable
`smartparens-strict-mode` to keep my s-expressions balanced even when I happen
to use `delete-char` or `kill-word` dangerously near a closing parenthesis.

I have `sp-kill-sexp` bound to <kbd>C-M-k</kbd>, however out of habit
I often use <kbd>C-k</kbd> to kill a line, which in my configuration is set up
as Artur Malabarba explained in his [Kill Entire Line with Prefix
Argument](http://endlessparentheses.com/kill-entire-line-with-prefix-argument.html). Doing
that in the middle of an s-expression creates unnerving chaos.

Smartparens comes with a handy binding to temporarily disable the enforced
balancing and let me insert a closing delimiter. Just pressing <kbd>C-q</kbd>
followed by the desired matching parenthesis brings the order back.

Unfortunately, it’s not always that easy. Take this snippet which appears at the
end of a ClojureScript function:

``` clojure
(when-not (empty? @data)
            [:div
             {:style {:padding "1em" :text-align "center"}}
             [graph]])]]))))
```

Carelessly hitting <kbd>C-k</kbd> near `[graph]` disrupts an otherwise elegant
s-expression. I could undo, of course, but what if after <kbd>C-k</kbd> I do
other kill-and-yank edits?

This is exactly why I have come to love
[syntactic-close](https://github.com/emacs-berlin/syntactic-close).

``` emacs-lisp
(use-package syntactic-close            ; Automatically insert closing delimiter
  :ensure t
  :bind ("C-c x c" . syntactic-close))
```

As soon as I discover an unbalanced s-expression, I can use <kbd>C-c x c</kbd>
as many times as needed to add back the right closing delimiters.
