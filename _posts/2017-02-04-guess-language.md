---
layout:     post
title:      Automatic language detection for Flyspell
date:       2017-02-04
summary:    How to improve spell-checking in Emacs with `guess-language`.
categories: emacs
---

I am always looking for new ways to improve my Emacs experience. Months ago, it
happened to my spell-checking configuration thanks to the work
of [Nicolas Petton](https://github.com/NicolasPetton).

``` emacs-lisp
(defvar mu-languages-ring nil "Languages ring for Ispell")

(let ((languages '("en_GB" "it_IT")))
  (validate-setq mu-languages-ring (make-ring (length languages)))
  (dolist (elem languages) (ring-insert mu-languages-ring elem)))

(defun mu-cycle-ispell-languages ()
  (interactive)
  (let ((language (ring-ref mu-languages-ring -1)))
    (ring-insert mu-languages-ring language)
    (ispell-change-dictionary language)))
```

This handy code lets me cycle quickly through the languages I need in
Flyspell. However, wouldn’t it be easier if Emacs was capable of automatic
language detection?

That’s the hole
the [guess-language](https://github.com/tmalsburg/guess-language.el) package
fills.

``` emacs-lisp
(use-package guess-language         ; Automatically detect language for Flyspell
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                   (it . ("it_IT" "Italian")))
        guess-language-languages '(en it)
        guess-language-min-paragraph-length 45)
  :diminish guess-language-mode)
```

Notice how I set `guess-language-langcodes`. It has to be set this way to make
`guess-language` work with my setup
for
[Hunspell](https://manuel-uberti.github.io/emacs/2016/06/06/spellchecksetup/).

Support for different languages and language detection for multiple languages in
the same document are provided out of the box. `guess-language` is still young,
but it’s already making spell-checking in Emacs so much easier.
