---
layout:     post
title:      ! 'Lockdown Beam: volatile-highlights'
date:       2020-04-27
summary:    Temporary highlights can be useful.
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
          volatile-highlights
      </li>
    </ul>
  </div>
</div>

In the middle of a frantic editing session one can use some help. You are there
sweating, firing key bindings with a smugness you can barely keep at bay, with
buffers and windows appearing and disappearing. Point is visibly tired of your
powerful skills because, let’s face it, jumping around is not always
pleasurable.

In situations such as this, you may find yourself killing a paragraph or a block
of code in order to yank it somewhere else. If that “somewhere else” is another
buffer (e.g., a Clojure file others worked on), I for one don’t want to make a
mess. This is precisely where Keitaro Miyazaki’s [volatile-highlights](https://github.com/k-talo/volatile-highlights.el) finds its
place.

Keitaro’s package adds a temporary highlight to the yanked text which makes the
addition stand out briefly, and so I get a sense of what I am doing in a buffer
I’m not entirely familiar with yet. There are other commands that benefit from
it, as the `volatile-highlights` README explains. For instance, I find the
highlights around undo operations and `occur-mode` occurrences very helpful.

Although I didn’t play with it, `volatile-highlights` can be extended to support
other packages and commands. Check out `vhl/define-extension` and
`vhl/install-extension `if you want to go down that road.

This is the last entry in my “Lockdown Beam” series. I was hoping to outlast the
lockdown, but I lost the battle eventually. Anyway, do give these packages a
chance. It’s easy to overlook a little gem in the ever-growing Emacs ecosystem.
