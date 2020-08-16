[![Build Status](https://travis-ci.org/Millak/my-guix.svg?branch=master)](https://travis-ci.org/Millak/my-guix)

Efraim's Guix Channel
=====================

A collection of custom Guix packages that aren't (yet) suitable
for submission upstream.

Usage
-----

This channel can be installed as a
[[https://www.gnu.org/software/guix/manual/en/html_node/Channels.html][Guix channel]].
To do so, add it to =~/.config/guix/channels.scm=:

#+BEGIN_SRC scheme
  (cons* (channel
          (name 'efraim-dfsg)
          (url "https://git.sr.ht/~efraim/my-guix")
          ;; Enable signature verification:
          (introduction
           (make-channel-introduction
            "cbc5f5e79732ddea5eb8a2456fce0e7c15c8fa23"
            (openpgp-fingerprint
             "A28B F40C 3E55 1372 662D  14F7 41AA E7DC CA3D 8351"))))
   %default-channels)
#+END_SRC

  Then run =guix pull=.

The packages in this repo will take precedence over those in the
official distribution.
