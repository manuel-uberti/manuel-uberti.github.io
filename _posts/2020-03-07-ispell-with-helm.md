---
layout:     post
title:      Spell-checking and Helm
date:       2020-03-07
summary:    A custom command to select dictionaries.
categories: emacs
---

Helm provides so many great commands I am still discovering new gems in it, like
the useful `helm-regexp`, which lets you build your regular expression pattern,
see the results of your search live in the Helm buffer, and optionally pass the
query to `query-replace-regexp`. So neat.

This is why I haven’t written a Helm command of my own until recently. To be
fair, as I am going to show there is not even the need for this command to
exist, but coding it has been a fun exercise anyway.

When it comes to spell-checking my buffers, I have been relying on
[mu-cycle-ispell-languages](https://www.manueluberti.eu/emacs/2017/02/04/guess-language/) to change dictionaries and [flyspell-correct-helm](https://github.com/d12frosted/flyspell-correct) to
pick the correct word I happened to misspell. Since I am already using Helm with
Flyspell, then, why not using it to select the dictionary too?

First, the actual dictionaries.

``` emacs-lisp
(defvar mu-dictionaries '(("en_GB" . "English")
                          ("it_IT" . "Italian")))
```

Note that I use `hunspell` for spell-checking as I explained in [Spell-checking
with Hunspell and flyspell-correct](https://www.manueluberti.eu/emacs/2016/06/06/spellchecksetup/).

Next, my new command.

``` emacs-lisp
(defun mu-select-dictionary ()
  "Select the dictionary for the spell-checker."
  (interactive)
  (helm :sources
        (helm-build-sync-source "Dictionaries"
          :candidates (map-values mu-dictionaries)
          :action (lambda (candidate)
                    (let ((dict (car (rassoc candidate mu-dictionaries))))
                      (ispell-set-spellchecker-params)
                      (setq ispell-dictionary dict
                            ispell-local-dictionary dict
                            ispell-local-dictionary-overridden t)
                      (ispell-internal-change-dictionary)
                      (setq ispell-buffer-session-localwords nil)
                      (run-hooks 'ispell-change-dictionary-hook))))
        :preselect (map-elt mu-dictionaries ispell-dictionary)
        :buffer "*helm select dictionary*")
  (flyspell-buffer))
```

I merely followed the guidelines on the [Helm wiki](https://github.com/emacs-helm/helm/wiki/Developing), but let’s break it down.

``` emacs-lisp
(helm :source
      (helm-build-sync-source "Dictionaries"
```

The source of my Helm command comes from the simple `alist` defined before
(`mu-dictionaries`), so `helm-build-sync-source` is enough.

``` emacs-lisp
:candidates (map-values mu-dictionaries)
```

Here I am using the excellent [map.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/map.el) to get the clearer “English” and “Italian”
labels as `:candidates` for the Helm buffer.

``` emacs-lisp
:action (lambda (candidate)
          (let ((dict (car (rassoc candidate mu-dictionaries))))
            (ispell-set-spellchecker-params)
            (setq ispell-dictionary dict
                  ispell-local-dictionary dict
                  ispell-local-dictionary-overridden t)
            (ispell-internal-change-dictionary)
            (setq ispell-buffer-session-localwords nil)
            (run-hooks 'ispell-change-dictionary-hook)))
```

The anonymous function for `:action` is a small version of
`ispell-change-dictionary`, containing only what I need for this operation to
complete successfully. Note the use of `rassoc`: this is how I get the key
corresponding to the value of `candidate` in `mu-dictionaries`. I didn’t find a
similar facility in `map.el`, but `rassoc` and `car` get the job done.

``` emacs-lisp
:preselect (map-elt mu-dictionaries ispell-dictionary)
```

Here I am using the current dictionary as the preselected element in the Helm
buffer. I could use `:input` to have it offered on the prompt, but I prefer to
have the prompt empty in this case.

If you look closely at `ispell-change-dictionary` source code, you can understand
the triviality of `mu-select-dictionary`: `ispell-change-dictionary` uses
`completing-read`, so in my case it already pops up a Helm buffer. But I am still
happy with this solution, and not just because I prefer “Dictionaries” instead
of “ispell-change-dictionary” as the source name displayed in my Helm buffer.
(Yes, of course it’s mostly because of it.)

As a bonus, thanks to `flyspell-buffer` the current buffer is spell-checked right
after the new dictionary is selected.
