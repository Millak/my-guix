[![Build Status](https://travis-ci.org/Millak/my-guix.svg?branch=master)](https://travis-ci.org/Millak/my-guix)

Efraim's Guix Channel
=====================

A collection of custom Guix packages that aren't (yet) suitable
for submission upstream.

Usage
-----

This channel can be installed as a
[Guix channel](https://www.gnu.org/software/guix/manual/en/html_node/Channels.html).
To do so, add it to '~/.config/guix/channels.scm':

```
  (cons* (channel
          (name 'efraim-dfsg)
          (url "https://git.sr.ht/~efraim/my-guix")
          ;; Enable signature verification:
          (introduction
           (make-channel-introduction
            "61c9f87404fcb97e20477ec379b643099e45f1db"
            (openpgp-fingerprint
             "A28B F40C 3E55 1372 662D  14F7 41AA E7DC CA3D 8351"))))
   %default-channels)
```

Then run 'guix pull'.

The packages in this repo will take precedence over those in the
official distribution.
