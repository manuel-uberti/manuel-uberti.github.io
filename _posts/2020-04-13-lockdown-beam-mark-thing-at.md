---
layout:     post
title:      ! 'Lockdown Beam: mark-thing-at'
date:       2020-04-13
summary:    Empower the built-in `thingatpt`.
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
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/04/06/lockdown-beam-hide-mode-line/">
          hide-mode-line
        </a>
      </li>
      <li>mark-thing-at</li>
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/04/20/lockdown-beam-native-complete/">
          native-complete
        </a>
      </li>
    </ul>
  </div>
</div>

Until I noticed that I was hardly using everything it has to offer, Leo
Liu’s [easy-kill](https://github.com/leoliu/easy-kill) was one of the packages I used the most. Specifically, I wasn’t
really taking advantage of its capability of repeating mark commands, nor its
keys to modify the current selection. Although it doesn’t seem actively
maintained this day, check it out if you want to extend your mark/kill actions.

As for me, Paul Landes’s [mark-thing-at](https://github.com/plandes/mark-thing-at) has become my best pal when it comes to
marking. Granted, it’s way less powerful than `easy-kill`, but combine it with
Emacs regular key bindings for killing and there is nothing more that I need.

You will find `mark-thing-at` on MELPA, and if you follow my lead, you can enable
`mark-thing-at-mode` and be happy with it. Note that if you do not configure
`mark-thing-at-keymap-prefix` you end up with <kbd>C-x m</kbd> as a prefix for all the `mark-*`
commands now available at your fingertips. This means that you will lose the key
binding for `compose-mail`, but I’m fine with it.

Whatever the prefix you end up with just append <kbd>C-h</kbd> to it and you will see the
many commands at your disposal. You probably won’t need them all, but it’s good
to have a nice way to leverage these marking facilities.

Next time we will explore a better completion system for `shell-mode`.

Stay safe.
