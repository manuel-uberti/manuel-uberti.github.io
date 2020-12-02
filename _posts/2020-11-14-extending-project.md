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
(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun mu--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan
   (lambda (dir)
     (mu--project-files-in-directory dir))
   (or dirs (list (project-root project)))))
```

`project.el` has to be made aware of my `local` type now.

``` emacs-lisp
(defun mu-project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))
```

`mu-project-try-local` just needs to be added to `project-find-functions` to
make sure my non-Git projects become known and remembered across sessions when
I hit <kbd>C-x p p</kbd>. This is way more elegant than the previous patch.

Since I also never use Git submodules, I can push my extensions a little further.

``` emacs-lisp
(defun mu--backend (dir)
  "Check if DIR is under Git, otherwise return nil."
  (when (locate-dominating-file dir ".git")
    'Git))

(defun mu-project-try-vc (dir)
  "Determine if DIR is a project.
This is a thin variant of `project-try-vc':
- It takes only Git into consideration
- It does not check for submodules"
  (let* ((backend (mu--backend dir))
         (root
          (when (eq backend 'Git)
            (or (vc-file-getprop dir 'project-git-root)
                (let ((root (vc-call-backend backend 'root dir)))
                  (vc-file-setprop dir 'project-git-root root))))))
    (and root (cons 'vc root))))
```

`mu-project-try-vc` now replaces `project-try-vc` in `project-find-functions`.

## Notes

[^1]: See [Generic Functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html) in the manual.
