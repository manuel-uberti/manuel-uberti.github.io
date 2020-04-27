---
layout:     post
title:      ! 'Lockdown Beam: hide-mode-line'
date:       2020-04-06
summary:    Hide the mode-line in certain windows.
categories: emacs
---

<div style="padding-bottom: 15px">
  <div class="box">
    <h3>Lockdown Beam</h3>
    <ul>
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/03/19/lockdown-beam-bm/">
          bm.el
        </a>
      </li>
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/03/23/lockdown-beam-eldoc-eval/">
          eldoc-eval
        </a>
      </li>
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/03/30/lockdown-beam-git-identity/">
          git-identity
        </a>
      </li>
      <li>hide-mode-line</li>
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/04/13/lockdown-beam-mark-thing-at/">
          mark-thing-at
        </a>
      </li>
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/04/20/lockdown-beam-native-complete/">
          native-complete
        </a>
      </li>
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/04/27/lockdown-beam-volatile-highlights/">
          volatile-highlights
        </a>
      </li>
    </ul>
  </div>
</div>

Fun fact: the mode-line is the one Emacs thing I’ve probably spent most of my
tinkering time on. Why? Hard to tell. I consider it a place for useful
information, but the only interaction I have with it is *looking* at it. It’s not
like some of those fancy packages offering you all manners of interactive
commands and assorted key bindings[^1]. It’s a, well, it’s a *line*.

Among the several iterations, [this one](https://www.manueluberti.eu/emacs/2018/03/10/moody-and-minions/) has lasted the longest. But then again,
not too long ago I felt the irresistible call of minimalism, and so I ended up
with [this other one](https://www.manueluberti.eu/emacs/2020/03/01/helm-ripgrep-mode-line/). And yet, despite the immoderate amount of care I spent on
it, there are times where I just want it out of my way.

For instance, I have no use for the mode-line in `shell-mode` or when I am reading
the latest news via Elfeed. Luckily, Henrik Lissner’s [hide-mode-line](https://github.com/hlissner/emacs-hide-mode-line) is out
there for this kind of messy work.

Provided the mode you don’t want to see the mode-line in has a hook, you can add
`#'hide-mode-line-mode` to it with [add-hook](http://doc.endlessparentheses.com/Fun/add-hook.html). For Elfeed, however, a different
solution is needed:

``` emacs-lisp
(defun mu-elfeed-open ()
  "Open elfeed and hide the mode-line."
  (interactive)
  (mu-save-window-config-then-call #'elfeed)
  (hide-mode-line-mode +1))
```

Have a look at [Passing the prefix argument around](http://127.0.0.1:4000/emacs/2019/06/21/windows/) if you want to know what
`mu-save-window-config-then-call` does.

Obviously you can go berserk and hide the mode-line everywhere with
Henrik’s package. But would you *really* do that? Don’t be cruel and think about
it.

Next time we are going to mark things, so keep your markers around.

Stay safe.

## Notes

[^1]: <kbd>M-x ^mode-line</kbd> actually gives me ten entries, so what do I know?
