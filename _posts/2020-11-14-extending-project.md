---
layout:     post
title:      Extending project.el
date:       2020-11-14
summary:    Leveraging `cl-defmethod`.
categories: emacs
---

In my [first appreciation](https://www.manueluberti.eu/emacs/2020/09/18/project/) of `project.el` I wrote about a patch for
`project--files-in-directory`. It’s a working solution, I won’t deny that, but
patching code always feels hacky. It’s like a dirty workaround you cannot avoid
to look at every time you visit your Emacs configuration.

By inspecting the code of `project.el` I noticed that `project-files` is a generic
function. In Emacs Lisp parlance, a generic function specifies an abstract
operation with the actual implementation provided by methods[^1]. This simply means
that I can devise my own implementation of `project-files`.

``` emacs-lisp
(defun mu--backend (dir)
  "Check if DIR is under Git, otherwise return nil."
  (when (locate-dominating-file dir ".git")
    'Git))

(defun mu--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head vc)) &optional dirs)
  "Override `project-files' to use `fd' in a non-Git project."
  (mapcan
   (lambda (dir)
     (let (backend)
       (if (and (file-equal-p dir (cdr project))
                (setq backend (mu--backend dir))
                (eq backend 'Git)
                (not project-vc-ignores))
           (project--vc-list-files dir backend project-vc-ignores)
         (mu--project-files-in-directory dir))))
   (or dirs
       (list (project-root project)))))
```

This is way more elegant than the previous patch. Not only I am now able to use
[fd](https://github.com/sharkdp/fd) in my projects, but I am restricting the VCS checks to Git since it is the
only VCS tool I use. This restriction is a bit brutal, I know, but it allows me
to skip checks for backends I never need and might slow things down.

Since I also never use Git submodules, I can push my simplifications a little
further.

``` emacs-lisp
(defun mu-project-try-vc (dir)
  "Determine if DIR is a project.
This is a thin variant of `project-try-vc':
- It takes only Git into consideration
- It does not check for submodules "
  (let* ((backend (mu--backend dir))
         (root
          (when (eq backend 'Git)
            (or (vc-file-getprop dir 'project-git-root)
                (let ((root (vc-call-backend backend 'root dir)))
                  (vc-file-setprop dir 'project-git-root root))))))
    (and root (cons 'vc root))))
```

`mu-project-try-vc` now replaces `project-try-vc` in `project-find-functions`.

Another thing I find useful is having `project.el` facilities for non-Git
projects. To do this, I place a `.project` file in the root directory of my
project and make `project.el` aware of this new type.

``` emacs-lisp
(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun mu-project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))
```

As you may have already guessed, `mu-project-try-local` just needs to be added to
`project-find-functions`. This makes sure my non-Git projects become known and
remembered across sessions when I hit <kbd>C-x p p</kbd>.

## Notes

[^1]: See [Generic Functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html) in the manual.
