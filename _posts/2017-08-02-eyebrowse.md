---
layout:     post
title:      A better workflow with workspaces
date:       2017-08-06
summary:    Organising window configurations with `eyebrowse`.
categories: emacs
---

I am not a big fan of workspaces on my desktop environments, maybe because to me
<kbd>Alt+Tab</kbd> is enough. To be honest, I was using workspaces back in my
[Ratpoison](http://www.nongnu.org/ratpoison/) days, but that is not the case any
more.

Nevertheless, my daily workflow in Emacs improved so much with
[eyebrowse](https://github.com/wasamasa/eyebrowse) I can’t imagine going back to
a configuration without it.

``` emacs-lisp
(use-package eyebrowse                  ; Easy workspaces creation and switching
  :ensure t
  :config
  (validate-setq eyebrowse-mode-line-separator " "
                 eyebrowse-new-workspace t)

  (eyebrowse-mode t))
```

As you can see, I am pretty much leaving `eyebrowse` with its default
settings. I removed the comma that was used as a separator between the workspace
numbers and made switching to a new workspace start from the `*scratch*` buffer.

Now my daily workflow in Emacs is thus organised:

- Workspace #1: this is dedicated to my GTD project, which simply follows the
  steps Nicolas Petton describes in [Orgmode for
  GTD](https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html).

- Workspace #2: this is for Clojure and ClojureScript development, so everything
  from CIDER to project specific Magit buffers and EShells ends up here. In the
  rare case I need to work on a different project, I create a new workspace.

- Workspace #3: this is the “I need a break!” place. Elfeed is my preferred
  choice for a quiet and informative distraction. Otherwise, if I just want to
  hack on my Emacs configuration I do it here.

Combined with [Projectile](https://github.com/bbatsov/projectile), `eyebrowse`
is the definitive piece of the puzzle I was missing to have an optimised work
environment.
