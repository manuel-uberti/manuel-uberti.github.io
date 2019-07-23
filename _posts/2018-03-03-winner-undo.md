---
layout:     post
title:      Refactoring my windows restoring solution
date:       2018-03-03
summary:    Better reusability with `funcall`.
categories: emacs
---

Following my [last
article](https://manuel-uberti.github.io/emacs/2018/02/24/restore-windows/) on
restoring window configurations with custom functions, [Clément
Pit-Claudel](https://github.com/cpitclaudel) got in touch with me and suggested
a little improvement. Clément pointed out the built-in Emacs command
[winner-undo](http://doc.endlessparentheses.com/Fun/winner-undo.html), which
I promptly used to simplify my solution.

Let’s enable `winner-mode`, first, because `winner-undo` comes with it.

``` emacs-lisp
(use-package winner                     ; Restore old window configurations
  :init (winner-mode t))
```

`winner-mode` automatically takes care of storing window configurations, thus
I can safely remove functions like `mu-ibuffer-open`:

``` emacs-lisp
(defun mu-ibuffer-open ()
  "Open Ibuffer after storing the current window configuration."
  (interactive)
  (mu-push-window-configuration)
  (ibuffer))
```

Now I only need to call `winner-undo` when I exit the desired, *fullframed*
buffer[^symbol].

``` emacs-lisp
(defun mu-pop-window-configuration ()
  "Kill current buffer and restore the previous window configuration."
  (interactive)
  (kill-this-buffer)
  (let ((inhibit-message t))
    (winner-undo)))
```

I wrapped `(winner-undo)` with `(inhibit-message t)` to get rid of the
superfluous message about the restored window configuration number.

This is a much cleaner solution. I still have to use
[fullframe](https://github.com/tomterl/fullframe), of course, but by reusing an
inner Emacs functionality I get the same result with less code.

However, this code has a problem. `winner-mode` does not save the configuration
of a frame with a single window. In this case `winner-undo` has nothing to
*undo* and it usually takes me back to a completely different buffer than the
one I started from.

Therefore I revised my functions and made them a bit more general. Basically,
I removed `mu-push-window-configuration` and added `mu-save-wins-then-call`.

``` emacs-lisp
(defun mu-save-wins-then-call (func &optional args)
  "Save current window configuration, then call FUNC optionally with ARGS."
  (interactive)
  (push (current-window-configuration) mu-saved-window-configuration)
  (cond
   ;; We have arguments for the function
   ((bound-and-true-p args) (funcall func args))
   ;; The function requires exactly one argument, and we want it to be nil
   ((equal args "nil") (funcall func nil))
   ;; The function does not expect arguments
   (t (funcall func))))
```

The `"nil" `check is kind of a hack just for `paradox-list-packages`, which
expects one non-optional argument. Since `&optional args` evaluates to `nil`
when non present, I need the check to avoid calling `(funcall func)` in this
particular case. I am sure there is a better solution, but I’ll leave that for
another time.

Nonetheless, functions like the previously mentioned `mu-ibuffer-open` becomes
cleaner.

``` emacs-lisp
(defun mu-ibuffer-open ()
  "Save window configuration and call `ibuffer'."
  (interactive)
  (mu-save-wins-then-call 'ibuffer))

(defun mu-elfeed-open ()
  "Save window configuration and call `elfeed'."
  (interactive)
  (mu-save-wins-then-call 'elfeed))

(defun mu-paradox-open ()
  "Save window configuration and call `paradox-list-packages'."
  (interactive)
  (mu-save-wins-then-call 'paradox-list-packages "nil"))
```

With the help of [funcall](http://doc.endlessparentheses.com/Fun/funcall.html)
I have a generic wrapper that can be applied wherever I need.

Thanks again to Clément Pit-Claudel who helped me dig further into my idea and
explore new paths.

<hr/>

[^symbol]: Read
    [here](https://manuel-uberti.github.io/emacs/2018/02/24/restore-windows/) to
        understand what *fullframed* means.
