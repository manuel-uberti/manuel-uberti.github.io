---
layout:     post
title:      ! 'Lockdown Beam: native-complete'
date:       2020-04-20
summary:    Better completion for `shell-mode`.
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
      <li>native-complete</li>
      <li>
        <a href="https://www.manueluberti.eu/emacs/2020/04/27/lockdown-beam-volatile-highlights/">
          volatile-highlights
        </a>
      </li>
    </ul>
  </div>
</div>

If you have followed this blog long enough, you know it’s been a while since [I
  embraced shell-mode](https://www.manueluberti.eu/emacs/2017/10/07/m-x-shell/). I even have a nice setup to [handle its windows](https://www.manueluberti.eu/emacs/2019/07/01/bury/) and to
  apply [better colours](https://www.manueluberti.eu/emacs/2019/06/14/xterm/) to it[^1].
  
However, before Troy Hinckley’s [native-complete](https://github.com/CeleritasCelery/emacs-native-shell-complete) fixed it for me, tab-completion
in `shell-mode` wasn’t quite the same experience as the one on a separate terminal
emulator. To be honest, I didn’t investigate far enough to understand whether
this was due to my Bash configuration or to Emacs itself. The only thing I know
is that Troy’s package got me where I wanted to be.

On MELPA you will find two packages: `native-complete` and
`company-native-complete`. I use both, but if you are not a Company user you can
live with the former alone. 

Anyway, I set up `native-complete` with a hook to `shell-mode` that runs
`native-complete-setup-bash`, then another hook adds `company-native-complete` to
`company-backends`. Tom explained everything you need to start and other useful
details in the package README, so head over there for more details. The only
thing I can add is that I have <kbd>TAB</kbd> bound to `company-complete` in `shell-mode-map`.

Now that there is a [libvterm integration](https://github.com/akermu/emacs-libvterm) for Emacs, staying with `shell-mode` may
sound anachronistic, because proper terminal emulation with your favourite shell
is just a package away. But if you, like me, are happy with Bash in `shell-mode`,
I can’t recommend `native-complete` and `company-native-complete` enough. By the
way, Troy’s work supports `csh` and other fancy things too, so you’re not strictly
limited to Bash.

Next time we will see highlights disappear suddenly. Is the Evil Genius[^2]
operating my Emacs? Who knows.

Stay safe.

## Notes

[^1]: Note that the [Modus Themes](https://www.manueluberti.eu/emacs/2020/03/16/modus-themes) have built-in support for that, so I don’t
    customise `xterm-color-names` and `xterm-color-names-bright` any more.

[^2]: [Evil Genius](https://en.wikipedia.org/wiki/Evil_demon).
