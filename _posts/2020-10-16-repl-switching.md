---
layout:     post
title:      Reliable REPL switching and buffer loading with CIDER
date:       2020-10-16
summary:    A do-what-I-desperately-mean approach.
categories: emacs
---

As a Clojure developer one of the most important packages in my everyday Emacs
usage is [CIDER](https://cider.mx/). There are many things to love about it, chief among them the
great interactive code evaluation and a sweet integration with popular Clojure
tools. I still haven’t played with its debugging facilities, but yes, I know I should.

However, there is something that has been bothering me for quite a while: REPL
switching and buffer loading. I cannot pinpoint the exact moment when these
seemingly simple operations have become so unreliable, but I still remember when
switching to the correct REPL buffer according to the file in front of me used
to work as expected and loading a buffer didn’t require dealing with sessions
first.

Let me give you more details before this writing starts to look like a random
rant. My projects are usually web applications, which means I have to write both
Clojure and ClojureScript. The Clojure side can be backed by `leiningen` or a
`deps.edn` file, and we are set on `shadow-cljs` for ClojureScript. The first thing
I normally do is jack-in with <kbd>C-c C-x j j</kbd> and then bring up the dedicated
ClojureScript REPL with <kbd>C-c C-x j s</kbd>. Opening the browser and navigate to
something along the lines of `localhost:3000` finalises the process of setting up
the ClojureScript REPL. That’s it, another good day of coding can begin. And
soon enough frustration follows.

I tend to move from Clojure to ClojureScript files and vice versa quite a lot,
and hitting <kbd>C-c C-z</kbd> frequently results in an unpredictable behaviour. Sometimes
the REPL of the expected type pops up, sometimes the other one appears,
sometimes I get a message about a missing REPL in the current session. Manually
linking the current buffer to the correct REPL with <kbd>C-c C-s b</kbd> seems to fix the
problem, but it’s only a matter of time. It takes a couple of other buffer
switching operations to bring the issue back. It is as if the link between the
buffer and the REPL has vanished. Even worse, without that link I can forget
about <kbd>C-c C-k</kbd> to load the buffer.

To overcome my frustration, I sat back and looked at how exactly I interact with
CIDER:

- I only deal with one project at a time
- I need at most two running REPLs
- I don’t really care about firing up dedicated REPLs for other projects. If
I change project, I simply close every buffer of the current one and start
afresh

This made me realise that the whole CIDER [session](https://docs.cider.mx/cider/0.26/usage/managing_connections.html#sessions) management is too much for my
basic needs.

``` emacs-lisp
(defun mu--cider-repl-regex (type)
  "Return the regexp to get the CIDER REPL based on TYPE."
  (if (eq type 'clj)
      "\`*cider-repl .*clj[*]"
    "\`*cider-repl .*cljs"))

(defun mu--cider-repl-buffer-name (type)
  "Get buffer from `buffer-list' according to TYPE."
  (let ((regex (mu--cider-repl-regex type)))
    (car (seq-filter (lambda (s) (string-match-p regex s))
                     (mapcar #'buffer-name (buffer-list))))))

(defun mu-cider-switch-to-repl (type)
  "Open a CIDER REPL for TYPE.
If TYPE is not passed, open a Clojure REPL."
  (interactive "P")
  (let ((type (or type 'clj)))
    (if-let (buffer (mu--cider-repl-buffer-name type))
        (pop-to-buffer buffer)
      (message "No CIDER REPL available"))))

(defun mu-cider-switch-to-cljs-repl ()
  "Open a CIDER REPL for ClojureScript."
  (interactive)
  (mu-cider-switch-to-repl 'cljs))
```

<kbd>C-c C-z</kbd> is bound to `mu-cider-switch-to-repl` in `clojure-mode-map` and
`clojurec-mode-map`, and to `mu-cider-switch-to-cljs-repl` in
`clojurescript-mode-map`. This of course means that in `.cljc` files I always get to
a Clojure REPL, but that’s fine. The code in there has to be tested on both
REPLs anyway, so it doesn’t matter which one comes up first.

Now, let’s fix <kbd>C-c C-k</kbd> as well.

``` emacs-lisp
(defun mu--cider-session-by-type (type)
  "Return the current CIDER session by TYPE."
  (let* ((regex (mu--cider-repl-regex type))
         (system (sesman--system))
         (sessions (sesman-current-sessions system '(buffer))))
    (car
     (seq-filter (lambda (s)
                   (string-match-p regex (buffer-name (cadr s))))
                 sessions))))

(defun mu-cider-load-buffer (type)
  "Load the current buffer according to TYPE.
If TYPE is not passed, default to Clojure."
  (interactive "P")
  (let* ((system (sesman--system))
         (buf (current-buffer))
         (type (or type 'clj))
         (session (mu--cider-session-by-type type)))
    (sesman--clear-links)
    (sesman-link-session system session 'buffer buf)
    (cider-load-buffer buf)))
    
(defun mu-cider-load-cljc-buffer ()
  "Load the current ClojureC buffer."
  (interactive)
  (mu-cider-load-buffer 'clj)
  (mu-cider-load-buffer 'cljs))

(defun mu-cider-load-cljs-buffer ()
  "Load the current ClojureScript buffer."
  (interactive)
  (mu-cider-load-buffer 'cljs))
```

Just like for <kbd>C-c C-z</kbd>, <kbd>C-c C-k</kbd> is now bound to these commands according to the
mode map. For the sake of completeness, I have also disabled both <kbd>C-c C-k</kbd> and
<kbd>C-c C-z</kbd> in `cider-mode-map` in order to avoid any kind of overshadowing by CIDER.


Note that this approach works well with my intended CIDER usage. It may not be
what you are looking for if you are experiencing the same problems with REPL
switching and buffer loading. Still, I have been using these commands for a
while now and I am happy with them. CIDER has become my trusted Clojure IDE
again.
