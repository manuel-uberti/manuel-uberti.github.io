---
layout:     post
title:      Recentre the buffer when leaving Swiper
date:       2016-05-30
summary:    How `swiper-action-recenter` helps keep the screen centred.
categories: emacs
---

I always talk enthusiastically about
[Ivy](https://github.com/abo-abo/swiper). As a once heavy Helm user, I remember
being afraid of regretting the complete switch to Ivy after a few days. It did
not happen, and since I made the move I have only found Ivy — and the related
packages Swiper and Counsel — consistently getting better and better.

Swiper has soon become my preferred in-buffer searching tool. Only one minor
thing was disturbing me. After selecting the right candidate with
<kbd>RET</kbd>, the screen scrolled unpredictably at the top, centre, or bottom
of the buffer.

This is due to calculations behind the scenes about windows size, as Oleh Krehel
explained
[here](https://github.com/abo-abo/swiper/issues/116#issuecomment-218381046). Fortunately,
he was kind enough to introduce a handy `defcustom`.

```emacs-lisp
;; Always recentre when leaving Swiper
(setq swiper-action-recenter t)
```

Now every time I leave Swiper the screen is left centred. Furthermore, it is
possible to keep the screen centred even during candidate selection in Swiper by
pressing <kbd>C-M-n</kbd> or <kbd>C-M-p</kbd> instead of <kbd>C-n</kbd> or
<kbd>C-p</kbd>.

I do not mind the scrolling when Swiper is active, but I do like having my eyes
focused on one point of the screen when I find what I am looking for.
