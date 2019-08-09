---
layout:     post
title:      Open Magit from Ibuffer
date:       2019-08-06
summary:    Call `magit-status` on the selected buffer.
categories: emacs
---

It’s amazing how many packages for our beloved editor exist out there.
[ELPA](https://elpa.gnu.org/) and [MELPA](https://melpa.org/) keep growing and
growing, and curiosity is always pushing me out to look for a new package to
try.

[Last year](https://manuel-uberti.github.io/emacs/2017/08/06/eyebrowse/) I wrote
about Eyebrowse, a cool workspace manager that has proven to be a reliable
friend until recently. Eyebrowse is a helpful extension, but as it turns out the
built-in Ibuffer is enough to make sense of all of the buffers available. I open
it with <kbd>C-x C-b</kbd>, which is one of the most used key bindings in my
everyday Emacs interactions.

Ibuffer presents a customizable list of buffers on which, much like
[Dired](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html),
we can apply different kinds of operations: filter, sort, group, mark, delete,
bury, visit. Just press <kbd>h</kbd> in Ibuffer to get an idea.

Since I mainly work on projects versioned on Git,
[ibuffer-vc](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html)
helps with grouping the buffers in a project-based fashion and operate on them.
For instance, to quickly close a project and its related buffers, mark the
project header for deletion with <kbd>d</kbd> and then press <kbd>x</kbd>.

One thing I was missing in Ibuffer was an integration with Magit. I wanted to
open `magit-status` in the project the current buffer (i.e., the buffer where
point is on) belongs to.

``` emacs-lisp
(defun mu-ibuffer-magit ()
  "Open `magit-status' for the current buffer."
  (interactive)
  (let ((buf (ibuffer-current-buffer t)))
    (magit-status (cdr (ibuffer-vc-root buf)))))
```

I’ve bound this function to <kbd>v</kbd> in `ibuffer-mode-map`. Note that
<kbd>v</kbd> was previously calling `ibuffer-do-view`, so you may want to pick
the key that suits you best.
