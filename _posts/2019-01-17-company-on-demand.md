---
layout:     post
title:      Make your company behave
date:       2019-01-17
summary:    Setting `company-mode` to display candidates on demand.
categories: emacs
---

Sometimes it just takes a little thing to make your day.

I have been using [company-mode](https://company-mode.github.io/) for ages,
becoming accustomed to the list of candidates popping up whenever the automatic
completion kicks in.

However, more often than not what I need is in-buffer, context-aware completion,
and in this case `dabbrev-expand` or `dabbrev-completion`, which I have bound to
<kbd>C-,</kbd> and <kbd>C-.</kbd> respectively, are enough.

It’s pretty simple to turn off `company-mode` automatic completion:

``` emacs-lisp
(setq company-idle-delay nil)
```

That’s it.

To get the list of candidates back when I need, I have `company-complete` bound
to <kbd>C-c c</kbd>.

Nice and easy.
