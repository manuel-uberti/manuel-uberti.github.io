---
layout:     post
title:      Spell-checking with Hunspell and flyspell-correct
date:       2016-06-06
summary:    How to configure Flyspell to work with Hunspell and Ivy.
categories: emacs
---

In order to simplify the portability of my Emacs setup, I devised a useful shell
script to install everything my needs.

Recently I abandoned `aspell` in favour of `hunspell`, so the script has been
updated accordingly. However, `hunspell` needs some dictionaries to work with
and they are not readily available as Debian packages, but as [LibreOffice
Extensions](http://extensions.libreoffice.org/). Therefore, I just need to copy
the files over to `/usr/share/hunspell`.

A quick test with `hunspell -D` shows all is set up correctly.

``` console
$ hunspell -D
[…]
AVAILABLE DICTIONARIES (path is not mandatory for -d option):
/usr/share/hunspell/en_GB
/usr/share/hunspell/it_IT
/usr/share/hunspell/en_US
[…]

```

Emacs Lisp takes care of the rest.

``` emacs-lisp
(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_GB")
```

I write both in English and Italian, so the spell-checker must be able to switch
dictionary on demand.

``` emacs-lisp
(bind-key "C-c I"
          (lambda ()
            (interactive)
            (ispell-change-dictionary "it_IT")
            (flyspell-buffer)))

(bind-key "C-c E"
          (lambda ()
            (interactive)
            (ispell-change-dictionary "en_GB")
            (flyspell-buffer)))
```

To make things nicer,
[flyspell-correct](https://github.com/d12frosted/flyspell-correct) makes
Flyspell propose the suggested corrections via the always reliable Ivy.

``` emacs-lisp
(use-package flyspell-correct-ivy
  :ensure t
  :demand t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-word-generic)))
```
