---
layout:     post
title:      A better approach at searching with Helm
date:       2020-03-13
summary:    Refactoring my ripgrepping solution.
categories: emacs
---

When I wrote about how I use `ripgrep` with Helm in [Ripgrepping with Helm](https://www.manueluberti.eu/emacs/2020/02/22/ripgrepping-with-helm/),
I overlooked something. Actually, let’s be honest. I didn’t study my usual
search habits like I should’ve had to.

Looking closely at my per-project searches, there are two patterns I tend to
follow:

- searching for the thing at point in the current project;
- searching for *something* in the current project.

The first pattern is somehow addressed by the previous incarnation of
`mu-helm-rg`, but it can be improved by leveraging `helm`’s `:input` option[^1].

For the second pattern `helm-sources-using-default-as-input` needs to be
temporarily disabled, otherwise `helm` would start with the thing at point as
`:default` and would run an initial search that most of the times is not what I
need. I can still pick up the thing at point with <kbd>M-n</kbd> if needed, but in this
case I prefer typing what I want to find.

To achieve this, I decided to reimplement `helm-grep-ag-1` according to my needs:

``` emacs-lisp
(defun mu--helm-rg (directory &optional with-tap type)
  "Build the Helm command for `mu-helm-rg'.

For DIRECTORY, WITH-TAP, and TYPE see `mu-helm-rg'. This command
disables `helm-sources-using-default-as-input' temporarily to
avoid the automatic search which starts when :default is set to
`thing-at-point' (the default behaviour). The search starts
automatically only with WITH-TAP."
  (let ((helm-sources-using-default-as-input nil)
        (command (helm-grep--ag-command))
        (input (when with-tap
                 (thing-at-point 'sexp ’no-properties))))
    (setq helm-source-grep-ag
          (helm-make-source (upcase command) 'helm-grep-ag-class
            :header-name (lambda (name)
                           (format "%s in %s"
                                   name (abbreviate-file-name directory)))
            :candidates-process (lambda ()
                                  (helm-grep-ag-init directory type))))
    (helm-set-local-variable 'helm-input-idle-delay helm-grep-input-idle-delay)
    (helm :sources 'helm-source-grep-ag
          :keymap helm-grep-map
          :history 'helm-grep-ag-history
          :input input
          :truncate-lines helm-grep-truncate-lines
          :buffer (format "*helm %s*" command))))
```

Now `mu-helm-rg` must be updated, but the changes are trivial. I only need to
consider a new parameter, `with-tap`, and call `mu--helm-rg` properly.

``` emacs-lisp
(defun mu-helm-rg (directory &optional with-tap with-types)
  "Search in DIRECTORY with RG.

With WITH-TAP, search for thing at point. With WITH-TYPES, ask
for file types to search in."
  (interactive "P")
  (require 'helm-adaptive)
  (mu--helm-rg (expand-file-name directory)
               with-tap
               (helm-aif (and with-types
                              (helm-grep-ag-get-types))
                   (helm-comp-read
                    "RG type: " it
                    :must-match t
                    :marked-candidates t
                    :fc-transformer 'helm-adaptive-sort
                    :buffer "*helm rg types*"))))
```

Finally, let’s see the new `mu-helm-rg` in action:

``` emacs-lisp
(defun mu-helm-project-search (&optional with-types)
  (interactive "P")
  (mu-helm-rg (mu--project-root) nil with-types))

(defun mu-helm-project-search-at-point (&optional with-types)
  (interactive "P")
  (mu-helm-rg (mu--project-root) t with-types))

(defun mu-helm-file-search (&optional with-types)
  (interactive "P")
  (mu-helm-rg default-directory nil with-types))
```

## Notes

[^1]: See the [Developing](https://github.com/emacs-helm/helm/wiki/Developing) section of the Helm wiki for more on this.
