---
layout:     post
title:      Ignore some buffers in Ivy
date:       2016-05-23
summary:    How to always ignore undesired buffers in `ivy-switch-buffer`.
categories: emacs
---

If you use the great
[company-statistics](https://github.com/company-mode/company-statistics) to
improve your experience with [company-mode](https://company-mode.github.io/),
chances are that you already noticed an entry called
`company-statistics-cache.el` when you switch buffers with your favourite key
binding.

Hitting <kbd>RET</kbd> on `company-statistics-cache.el` could result in
unpleasant consequences. In my case, Emacs hangs indefinitely, distracting me
from my work and forcing me to a horrible restart.

Fortunately, [Oleh Krehel](https://github.com/abo-abo) introduced a new value
for `ivy-use-ignore-default`: `'always`.

```emacs-lisp
;; Always ignore buffers set in `ivy-ignore-buffers'
(setq ivy-use-ignore-default 'always)
;; Ignore some buffers in `ivy-switch-buffer'
(setq ivy-ignore-buffers '("company-statistics-cache.el"))
```

With the above setup, `company-statistics-cache.el` will never sneak in your
buffer switching again.
