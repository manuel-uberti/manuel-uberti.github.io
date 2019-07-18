---
layout:     post
title:      Format XML like a pro
date:       2019-07-18
summary:    Using `reformatter` to format XML buffers.
categories: emacs
---

[Years ago](https://manuel-uberti.github.io/emacs/2016/12/03/xmllint/), I wrote
about a custom function that uses
[`shell-command-on-region`](http://doc.endlessparentheses.com/Fun/shell-command-on-region.html)
to run `xmllint` on an XML buffer.

There is nothing wrong with this function, but a simpler approach is using
[reformatter](https://github.com/purcell/reformatter.el), a nice tool from the
prolific mind of Steve Purcell.

Once installed, defining a new formatter is trivial:

``` emacs-lisp
(reformatter-define xml-format
  :program "xmllint"
  :args '("--format" "-")
  :mode nil)
```

Now I can add a key binding for `xml-format` or simply type <kbd>M-x
xml-format</kbd> in an XML buffer to have it nicely printed.

Note that setting `:mode` to `nil` tells `reformatter-define` to not create a
minor mode, which I don’t need in this particular case. The minor mode could be
useful if you want your formatter to be called automatically when you save a
buffer. For instance, without changing the default setting for `:mode` you could
set up the formatter in a project via `.dir-locals.el` with:

``` emacs-lisp
((nxml-mode
  (mode . xml-format-on-save)))
```

You can also specify other options when creating a formatter, so be sure to
check the documentation of `reformatter-define` (<kbd>C-h f
reformatter-define</kbd>).
