---
layout:     post
title:      Restoring the window configuration in Emacs
date:       2018-02-24
summary:    Custom functions to never lose your windows setup.
categories: emacs
---

[Last time](https://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/)
I explained how I customised Magit to kill its buffers upon quitting
`magit-status`. My solution uses `magit-restore-window-configuration`, which
does exactly what the name suggests. Wouldn’t it be great if a similar handy
trick could be applied to any Emacs functionality?

Specifically:

- I’d like to run some modes in a window that takes up the whole frame
- I’d like to quit them with the previous window configuration completely
restored

Two modes that I use daily would definitely become more pleasant: `ibuffer-mode`
and `org-agenda-mode`. Fortunately, the Emacs community has already devised
everything I need.

First, I have to save the current window configuration.

``` emacs-lisp
(defvar mu-saved-window-configuration nil)

(defun mu-push-window-configuration ()
  "Save current window configuration."
  (interactive)
  (push (current-window-configuration) mu-saved-window-configuration))
```

I can now create a command to open the desired mode.

``` emacs-lisp
(defun mu-ibuffer-open ()
  "Open Ibuffer after storing current window configuration."
  (interactive)
  (mu-push-window-configuration)
  (ibuffer))
```

A buffer can be visited in a single window per frame with
[fullframe](https://github.com/tomterl/fullframe).

``` emacs-lisp
(with-eval-after-load 'ibuffer
    (fullframe ibuffer mu-pop-window-configuration))
```

The first argument to `fullframe` indicates that I want the function `ibuffer`
to be executed in a single window of the current frame. The second argument is
the command[^symbol] invoked when quitting Ibuffer, which will have to restore
the previous window configuration.

``` emacs-lisp
(defun mu-restore-window-configuration (config)
  "Kill current buffer and restore window configuration in CONFIG."
  (interactive)
  (kill-this-buffer)
  (set-window-configuration config))

(defun mu-pop-window-configuration ()
  "Restore previous window configuration and clear current window."
  (interactive)
  (let ((config (pop mu-saved-window-configuration)))
    (if config
        (mu-restore-window-configuration config)
      (if (> (length (window-list)) 1)
          (delete-window)
        (bury-buffer)))))
```

These little functions are just my *personal take* on the awesome gems
I discovered in [John Wiegley’s Emacs
configuration](https://github.com/jwiegley/dot-emacs/commit/654e2dd5a8667cff58061c3212b787720fc04804).

Nothing can stop me now from applying the same pattern to any mode I want. Once
again, Emacs shows a degree of customizability second to none.

<hr/>

[^symbol]: The symbol of a command, to be precise.
