---
layout:     post
title:      ! 'Lockdown Beam: eldoc-eval'
date:       2020-03-23
summary:    Extend Eldoc to the mode-line.
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
      <li>eldoc-eval</li>
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
    </ul>
  </div>
</div>

As mentioned briefly at the end of the [previous entry](https://www.manueluberti.eu/emacs/2020/03/19/lockdown-beam-bm/), today we are going to
play with Emacs’ [Eldoc](http://doc.endlessparentheses.com/Fun/eldoc-mode.html). Sometimes I forget about it, but `eldoc-mode` is one of
those subtle things which improve my daily Emacs routine. Besides giving me
useful information in Elisp, `eldoc-mode` is always helpful when I am programming
in Clojure thanks to its integration with CIDER.

However, there is another place where Eldoc comes in handy. You surely know that
when you press <kbd>M-:</kbd> you can type an expression in the minibuffer and then
evaluate it by pressing <kbd>RET</kbd>. But wouldn’t it be great to have Eldoc for these
quick runs to?

The answer is not “Well, yes!”, but “Hey, there is a mode for it!”, which is
what an Emacser regularly replies when asked about any matter whatsoever. This
time the mode comes with the package of the day: Thierry Volpiatto’s [eldoc-eval](https://github.com/thierryvolpiatto/eldoc-eval).

Once installed, activating `eldoc-eval` is easy. Turn on `eldoc-in-minibuffer-mode`
and everything is set.

<div style="text-align: center; padding-bottom: 15px">
    <a href="https://raw.githubusercontent.com/manuel-uberti/manuel-uberti.github.io/master/images/eldoc-eval.png" 
       target="_blank">
      <img src="/images/eldoc-eval.png">
    </a>
</div>

Nice and simple. You may not like the Eldoc information appearing in the
mode-line, but Thierry got you covered with `eldoc-in-minibuffer-show-fn`. If you
use tooltips, try setting this to `#'tooltip-show`. There are other features in
`eldoc-eval` worth of notice, so be sure to check out its README if you want more
juice.

As for me, I am happy with just enabling `eldoc-in-minibuffer-mode`. I patched
`eldoc-show-in-mode-line` to fit the help message properly in my custom mode-line,
but that’s it. If you are using your own mode-line too, I am leaving that as a
little exercise[^1].

Next time we will meet a Git-related package which doesn’t include the letters
“m” and “a” in its name.

Stay safe.

## Notes

[^1]: I used `el-patch` for this.
