---
layout:     post
title:      ! 'Ripgrepping with Helm: the mode-line'
date:       2020-03-01
summary:    Patching `helm-grep-ag-init` to my own taste.
categories: emacs
---

[Last time](https://www.manueluberti.eu/emacs/2020/02/22/ripgrepping-with-helm/) I tailored my own `helm-do-grep-ag` to show you how I use `ripgrep` within
Helm. However, I missed something: the mode-line.

The default mode-line Helm offers for `helm-do-grep-ag` doesn’t fit well with my
custom one, which is a minimal[^1] version of the excellent [doom-modeline](https://github.com/seagle0128/doom-modeline). It’s a
two-fold issue: my mode-line is bigger than the default one and I don’t need all
the information usually printed by `helm-do-grep-ag`.

By inspecting Helm’s source code one can easily find out what `helm-do-grep-ag`
does on [mode-line-format](http://doc.endlessparentheses.com/Var/mode-line-format.html). Basically, among other things `helm-grep-ag-init` sets
up the value for `mode-line-format` according to the number of results. Very well,
then, it’s time to bring [el-patch](https://www.manueluberti.eu/emacs/2019/12/01/el-patch/) back in the game.

First, though, aesthetics.

``` emacs-lisp
(set-face-bold 'helm-grep-finish t)
```

I am just making `helm-grep-finish` bold, nothing serious.

``` emacs-lisp
(el-patch-defun helm-grep-ag-init (directory &optional type)
  "Start AG process in DIRECTORY maybe searching only files of type TYPE."
  (let ((default-directory (el-patch-swap
                             (or helm-ff-default-directory
                                 (helm-default-directory)
                                 default-directory)
                             (or (mu--project-root)
                                 (helm-default-directory)
                                 default-directory)))
        (cmd-line (helm-grep-ag-prepare-cmd-line
                   helm-pattern (or (file-remote-p directory 'localname)
                                    directory)
                   type))
        (el-patch-remove
          (start-time (float-time)))
        (proc-name (helm-grep--ag-command)))
    (set (make-local-variable 'helm-grep-last-cmd-line) cmd-line)
    (el-patch-remove
      (helm-log "Starting %s process in directory `%s'"
                proc-name directory)
      (helm-log "Command line used was:\n\n%s"
                (concat ">>> " cmd-line "\n\n")))
    (prog1
        (start-file-process-shell-command proc-name helm-buffer cmd-line)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err (process-exit-status process))
                (noresult (= err 1))
                (el-patch-add
                  (proc (concat " HELM " proc-name))))
           (el-patch-swap
             (cond (noresult
                    (with-helm-buffer
                      (insert (concat "* Exit with code 1, no result found,"
                                      " command line was:\n\n "
                                      (propertize helm-grep-last-cmd-line
                                                  'face 'helm-grep-cmd-line)))
                      (setq mode-line-format
                            `(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      (format
                                       "[%s process finished - (no results)] "
                                       ,(upcase proc-name))
                                      'face 'helm-grep-finish))))))
                   ((string= event "finished\n")
                    (helm-log "%s process finished with %s results in %fs"
                              proc-name
                              (helm-get-candidate-number)
                              (- (float-time) start-time))
                    (helm-maybe-show-help-echo)
                    (with-helm-window
                      (setq mode-line-format
                            `(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      (format
                                       "[%s process finished in %.2fs - (%s results)] "
                                       ,(upcase proc-name)
                                       ,(- (float-time) start-time)
                                       (helm-get-candidate-number))
                                      'face 'helm-grep-finish))))
                      (force-mode-line-update)
                      (when helm-allow-mouse
                        (helm--bind-mouse-for-selection helm-selection-point))))
                   (t (helm-log
                       "Error: %s %s"
                       proc-name
                       (replace-regexp-in-string "\n" "" event))))
             (if noresult
                 (with-helm-buffer
                   (setq mode-line-format
                         `(""
                           mu-mode-line-bar
                           " "
                           (:eval
                            (propertize
                             (format "%s [no results]" ,proc)
                             'face 'helm-grep-finish)))))
               (with-helm-window
                 (setq mode-line-format
                       `(""
                         mu-mode-line-bar
                         " "
                         (:eval
                          (propertize
                           (format "%s [%s results]"
                                   ,proc (helm-get-candidate-number))
                           'face 'helm-grep-finish))))
                 (force-mode-line-update))))))))))
```

Let’s break down my changes:

``` emacs-lisp
(default-directory (el-patch-swap
                     (or helm-ff-default-directory
                         (helm-default-directory)
                         default-directory)
                     (or (mu--project-root)
                         (helm-default-directory)
                         default-directory)))
```

This has nothing do to with the mode-line, actually, but why not re-use
[mu–-project-root](https://www.manueluberti.eu/emacs/2020/02/22/ripgrepping-with-helm/) here? Exactly.

I also removed all the `helm-log` references, because I am confident my Elisp-fu
will not need debugging[^2]. Anyway, I want the buffer identification to look like
the rest of the Helm buffer identifications in my mode-line:

``` emacs-lisp
(proc (concat " HELM " proc-name))
```

I am going to use only `ripgrep` for my searches, so `proc-name` is usually `rg` here,
but one never knows what will come next.

When there are no results to be shown, I don’t want a message in the Helm
buffer, just a clean feedback:

``` emacs-lisp
(setq mode-line-format
      `((:eval mu-mode-line-bar)
        " "
        (:eval
         (propertize
          (format "%s [no results]" ,proc)
          'face 'helm-grep-finish))))
```

The same goes for one or more results:

``` emacs-lisp
(setq mode-line-format
      `((:eval mu-mode-line-bar)
        " "
        (:eval
         (propertize
          (format "%s [%s results]"
                  ,proc
                  (helm-get-candidate-number))
          'face 'helm-grep-finish))))
```

`mu-mode-line-bar` is what makes my mode-line bigger and I borrowed it from
`doom-mode-line`, so go check its fantastic code to find out about this magic
trick.

Finally, this is how the mode-line looks after <kbd>M-?</kbd> when the `ripgrep` process
finishes:

<figure>
    <img src="/images/helm-rg-finished.png">
</figure>

And for the sake of completeness, this is how it looks when I move through the
results with <kbd>C-p</kbd> and <kbd>C-n</kbd>:

<figure>
    <img src="/images/helm-rg.png">
</figure>

By the way, the colours are courtesy of [modus-vivendi](https://gitlab.com/protesilaos/modus-themes), a great theme by
Protesilaos Stavrou I’ve recently switched to.

## Notes ##

[^1]: As in “what is enough for me?”

[^2]: It’s not that I am *that* good, it’s just that I can worry about debugging
    later.
