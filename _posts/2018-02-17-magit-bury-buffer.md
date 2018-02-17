---
layout:     post
title:      Exterminate Magit buffers
date:       2018-02-17
summary:    How to leverage `magit-bury-buffer-function`.
categories: emacs
---

Some say that Emacs users rarely kill buffers. Instead they smoothly dance away
to another buffer using their preferred switching mechanism, and just keep on
working with no hiccups whatsoever. In my case `ivy-switch-buffer` is usually
doing all the the dancing, with
[Ibuffer](http://doc.endlessparentheses.com/Fun/ibuffer-mode.html) coming in
when the situation gets out of hand.

However, I do not enjoy having more open buffers than what I really need. Call
it obsessive-compulsive disorder, call it whatever you like, I want my buffers
list clean and on point.

[Magit](https://manuel-uberti.github.io/emacs/2017/11/09/magit/) has a penchant
for open buffers. Jonas Bernoulli already explained [the reasoning behind this
behaviour](https://github.com/magit/magit/issues/2124#issuecomment-125987469),
but he also suggested a [solution](https://emacs.stackexchange.com/a/35832/5514)
to clean up Magit-related buffers when the work is done.

Following Jonas’ tip, this is what I devised:

``` emacs-lisp
(defun mu-magit-kill-buffers (param)
  "Restore window configuration and kill all Magit buffers."
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(validate-setq magit-bury-buffer-function #'mu-magit-kill-buffers)
```

The function is simple: first it collects the available Magit buffers, then
restores the window configuration as it was before calling `magit-status`, and
finally applies `kill-buffer` on the buffers previously collected. `kill-buffer`
will not ask for confirmation, so now pressing <kbd>q</kbd> in `magit-status`
has the desired effect.

Something is weird in that function, though. Why pass `param` to
`mu-magit-kill-buffers` if there is no need for it in the function body?

Let’s break this down. The value of `magit-bury-buffer-function` is a
function[^symbol].

``` emacs-lisp
(defcustom magit-bury-buffer-function 'magit-restore-window-configuration
  "The function used to bury or kill the current Magit buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type '(radio (function-item quit-window)
                (function-item magit-mode-quit-window)
                (function-item magit-restore-window-configuration)
                (function :tag "Function")))
```

Looking at the code of `magit-restore-window-configuration`, we can see that it
accepts an optional argument.

``` emacs-lisp
(defun magit-restore-window-configuration (&optional kill-buffer)
  "Bury or kill the current buffer and restore previous window configuration."
  (let ((winconf magit-previous-window-configuration)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq magit-previous-window-configuration nil))))))
```

Thus `param` in `mu-magit-kill-buffers` is only there to match the signature of
the function passed to `magit-bury-buffer-function`. If I leave it out, I will
get this error message:

``` emacs-lisp
magit-mode-bury-buffer: Wrong number of arguments: ((t) nil "Kill all Magit buffers." (let ((buffers (magit-mode-get-buffers))) (magit-restore-window-configuration) (mapc #'kill-buffer buffers))), 1
```

If you’re wondering why I am using the sharp quote (`#'`) when setting the value
of `magit-bury-buffer-function`, you can simply trust [the sage
advice](http://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html)
of Artur Malabarba.

There is still one problem to solve. Setting `magit-bury-buffer-function` this
way makes <kbd>q</kbd> always kill *every* Magit buffer. This is not what I need
when I am using, for instance, `magit-log-buffer-file`.

Hence it’s better to make my custom function
[interactive](https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html)
and apply it only for `magit-status-mode-map`:

``` emacs-lisp
(defun mu-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map)
```

Bonus point: the unused `param` is gone.

<hr/>

[^symbol]: Actually it is a symbol, which in this case is the quoted name of a
    function.
