---
layout:     post
title:      Ripgrepping with Helm
date:       2020-02-22
summary:    Quickly search in projects and files
categories: emacs
---

Sometimes curiosity leads me to use alternative packages for built-in solutions
which I should have explored better. For instance, it took me moving through
[helm-ag](https://github.com/syohex/emacs-helm-ag) and
[helm-rg](https://github.com/cosmicexplorer/helm-rg)[^1] before realising Helm
already comes with a functionality suitable for my needs: `helm-do-grep-ag`.

By now I expect you to be familiar with the wonders of
[ripgrep](https://github.com/BurntSushi/ripgrep). I was heavily relying on
`counsel-rg` back in my Ivy days, and I needed a similar command in Helm. Truth
be told, `helm-do-grep-ag` is not the replacement I was looking for, but
understanding its code helped me devise my own utilities for super fast
searches.

First of all, let’s use `ripgrep` instead of `ag`:

``` emacs-lisp
(setq helm-grep-ag-command (concat "rg"
                                   " --color=always"
                                   " --smart-case"
                                   " --no-heading"
                                   " --line-number %s %s %s")
      helm-grep-file-path-style 'relative)
```

Note that the string for `helm-grep-ag-command` comes from its documentation.
I chose `relative` for `helm-grep-file-path-style` because I find relative file
paths in the search result buffer more useful than their base name.

Now, `helm-do-grep-ag` defers to `helm-grep-ag`, which in turns defers to
`helm-grep-ag-1`, and this is the Helm facility I can leverage for my own
`ripgrep` variant.

``` emacs-lisp
(defun mu-helm-rg (directory &optional with-types)
  "Search in DIRECTORY with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (require 'helm-adaptive)
  (helm-grep-ag-1 (expand-file-name directory)
                  (helm-aif (and with-types
                                 (helm-grep-ag-get-types))
                      (helm-comp-read
                       "RG type: " it
                       :must-match t
                       :marked-candidates t
                       :fc-transformer 'helm-adaptive-sort
                       :buffer "*helm rg types*"))))
```

It’s basically `helm-grep-ag` with minor aesthetic changes, but [you know
me](https://manuel-uberti.github.io/emacs/2019/12/01/el-patch/). Note the
`with-types` parameter. If I use this command with `C-u` I get to pick one or
more file types to narrow the search down to. I don’t remember if this is
possible with `counsel-rg`, but in large projects it is an absolute must have.
Furthermore, the symbol at point, if any, is the default search parameter. Why
is this awesome? Keep reading.

With `mu-helm-rg` in place, I can build a couple of commands to make my life
easier.

``` emacs-lisp
(defun mu-helm-project-search (&optional with-types)
  "Search in current project with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg (mu--project-root) with-types))

(defun mu-helm-file-search (&optional with-types)
  "Search in `default-directory' with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg default-directory with-types))
```

In case you are wondering, `mu–-project-root` is a trivial function to figure
out the current project root directory.

``` emacs-lisp
(defun mu--project-root ()
  "Return the project root directory or `helm-current-directory'."
  (require 'helm-ls-git)
  (if-let (dir (helm-ls-git-root-dir))
      dir
    (helm-current-directory)))
```

I don’t need `mu-helm-file-search` as often as `mu-helm-project-search`, which
is why I’ve bound the latter to <kbd>M-?</kbd>. I use this key binding so much
it must be a quick one. And this is why having the symbol at point automatically
picked up by `mu-helm-rg` is so important: it just makes the whole search
operation as simple as possible.

Oh wait, the symbol at point is not what you are after? No problem, you can
start typing whatever you want and have Helm *ripgrepping* your files.

## Notes ##

[^1]: Both packages seem hardly maintained these days.
