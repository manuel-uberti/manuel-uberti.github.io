---
layout:     post
title:      ! 'Lockdown Beam: git-identity'
date:       2020-03-30
summary:    Quickly set your identity in a Git repository.
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
      <li>git-identity</li>
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
    </ul>
  </div>
</div>

The situation doesn’t seem to get any better, does it? We are still confined in
our homes, still wary about our movements and contacts. Since not even running
outside is allowed, I am getting lazy, to the point that I’ve never thought I’d
miss the gym so much. It’s time to resort to in-house crunches and push-ups,
I suppose.

Meanwhile, let’s not forget about Emacs. Last time I hinted at a Git-related
package, but at this point wouldn’t writing about Magit again be out of
fashion[^1]?

When it comes to Git repository, the only thing I am not using Magit for is
checking and setting my identity. I’m not sure this is a common problem, but
I have to handle two Git identities on the same machine, one for personal
repositories and one for work projects. 

Without Akira Komamura’s [git-identity](https://github.com/akirak/git-identity.el) this situation was leading me to
confusion, because more often than not I found myself committing and pushing
work stuff with my personal identity. One could manually edit `.git/config` every
time or use Git’s own commands to deal with this, but to me these are both
cumbersome alternatives compared to the simplicity of Konamura’s package.

The only configuration needed for me after the installation was binding <kbd>I</kbd> to
`git-identity-info` in `magit-status-mode-map`. Now once I cloned a new repository
I can easily check whether the identity is correct or not.

<div style="text-align: center; padding-top: 5px; padding-bottom: 5px">
    <a href="https://raw.githubusercontent.com/manuel-uberti/manuel-uberti.github.io/master/images/git-identity.png" 
       target="_blank">
      <img src="/images/git-identity.png">
    </a>
</div>

Furthermore, `git-identity` can help even if for some random reason I didn’t rely
on `git-identity-info`. With `git-identity-magit-mode` enabled, `git-identity` makes
sure that on `magit-commit` a global or a local identity is actually set.

Next time we are going to play hide and seek with the mode-line.

Stay safe.

## Notes

[^1]: I am kidding, Magit is always stylish.
