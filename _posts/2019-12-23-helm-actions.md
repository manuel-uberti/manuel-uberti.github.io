---
layout:     post
title:      Developing Helm actions
date:       2019-12-23
summary:    A `helm-projects-history` with custom actions.
categories: emacs
---

One of the central pieces of my recent [return to
Helm](https://manuel-uberti.github.io/emacs/2019/11/16/helm/) is
[helm-ls-git](https://github.com/emacs-helm/helm-ls-git), which empowers
`helm-browse-project` with everything I need to move through my Git projects
smoothly. Combine that with `helm-projects-history`, and you can see why I don’t
need Projectile any more. Well, sort of. It’s true that I’ve stopped using
Projectile since Helm took over my Emacs configuration, but it’s also true that
I had to code the “helmified” version of a couple of features I was heavily
relying upon.

In Helm, you can leverage
[actions](https://github.com/emacs-helm/helm/wiki/Developing#writing-actions) to
extend the available utilities.

``` emacs-lisp
(defun mu--candidate-directory (candidate)
  "Find the directory containing CANDIDATE.
Default to `default-directory' if it cannot be found."
  (if (fboundp 'helm-ls-git-root-dir)
      (let ((dir (cond ((stringp candidate)
                        (file-name-directory candidate))
                       ((and (bufferp candidate)
                             (buffer-file-name candidate))
                        (file-name-directory (buffer-file-name candidate)))
                       (t default-directory))))
        (helm-ls-git-root-dir dir))
    (helm-current-directory)))

(defun mu-helm-project-dired ()
  "Open Dired buffers at project root from a Helm session."
  (interactive)
  (dolist (cand (helm-marked-candidates))
    (dired (mu--candidate-directory cand))))

(defun mu-helm-project-shell ()
  "Open shell buffers at project root from a Helm session."
  (interactive)
  (dolist (cand (helm-marked-candidates))
    (let* ((default-directory (mu--candidate-directory cand))
           (buffer (concat "*shell "
                           (file-name-nondirectory
                            (directory-file-name default-directory))
                           "*")))
      (mu-shell-open buffer))))
```

The actions are self-explanatory (see [Passing the prefix argument
around](https://manuel-uberti.github.io/emacs/2019/06/21/windows/) for
`mu-shell-open`). I often need to jump to a Dired or a shell-mode buffer of a
project different from the current one, so I want the possibility to use the
current candidate in `helm-projects-history` as a starting point.

To be fair, `mu--candidate-directory` does a little more than what I need here.
That’s because I use `mu-helm-project-dired` and `mu-helm-project-shell` in
`helm-buffers-list` and `helm-browse-project` too. The latter lists special
buffers (e.g., `*scratch*`) as buffers of the current project. Since they do not
live in any specific directory,
[default-directory](http://doc.endlessparentheses.com/Var/default-directory.html)
is the obvious choice.

Now I have to plug the actions into `helm-projects-history`. Unfortunately, it
doesn’t have a `keymap` to extend, so I need another solution.

``` emacs-lisp
(defvar mu-helm-projects-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c d") (helm-exit-and-run! (mu-helm-project-dired)))
    (define-key map (kbd "C-c s") (helm-exit-and-run! (mu-helm-project-shell)))
    map)
  "Keymap for `mu-helm-projects-history'.")

(defun mu-helm-projects-history (arg)
  "A `helm-projects-history' with custom actions."
  (interactive "P")
  (require 'helm-ls-git)
  (helm :sources
        (helm-build-sync-source "Project history"
          :candidates helm-browse-project-history
          :action (lambda (candidate)
                    (with-helm-default-directory candidate
                      (helm-browse-project
                       (or arg helm-current-prefix-arg))))
          :keymap mu-helm-projects-history-map)
        :buffer "*helm browse project history*"))
```

`mu-helm-projects-history` adds only two things to the original
`helm-projects-history`. I require `helm-ls-git` early because I need its
capabilities inside my actions, and I use my own `mu-helm-projects-history-map`
to make them available in the Helm buffer opened by this command.

Last but not least, `helm-exit-and-run!` is a helpful macro to avoid dry
repetitions:

``` emacs-lisp
(defmacro helm-exit-and-run! (&rest body)
  "Define an action with BODY to run after exiting Helm."
  (declare (doc-string 1))
  `(lambda ()
     (interactive)
     (with-helm-alive-p
       (helm-exit-and-execute-action (lambda (_candidate) ,@body)))))
```

Note that in my actions I am ignoring the `_candidate` parameter on purpose.
Since `helm-marked-candidates` grabs the current candidate when none of them is
selected, I can use my actions on multiple candidates if I feel wild enough.
Yeah, sometimes I can be crazy like that.
