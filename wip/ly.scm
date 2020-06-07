;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is an addendum to GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (wip ly)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages xorg))

(define-public ly
  (package
    (name "ly")
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cylgom/ly")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "04cw5ihr5inaxdwiz1302xvw5d0jibh49sin55i1w19045hil7yy"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f  ; no tests
       #:make-flags
       (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script
         (add-after 'unpack 'unpack-submodules
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each
               (lambda (submodule)
                 (copy-recursively (assoc-ref inputs submodule)
                                   (string-append "sub/" submodule)))
               '("argoat" "configator" "ctypes" "dragonfail" "termbox_next"))
             #t))
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tput (string-append
                           (assoc-ref inputs "ncurses") "/bin/tput")))
               (substitute* "makefile"
                 (("/usr") ""))
               (substitute* "src/main.c"
                 (("tput ") (string-append tput " ")))
               #t)))
         (replace 'build
           (lambda _
             (invoke "make" "final"))))))
    (native-inputs
     `(;; The following are all submodules for ly:
       ("argoat"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/cylgom/argoat")
                  (commit "36c41f09ecc2a10c9acf35e4194e08b6fa10cf45")))
           (file-name "argoat-for-ly")
           (sha256
            (base32
             "14v01fwm2bcgqh5b25iqfy4dm8pap8r49r1yfsr5c9j3b3hfn4yg"))))
       ("configator"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/cylgom/configator")
                  (commit "8227b3a835bf4c7e50a57e4ad6aff620ba0dc349")))
           (file-name "configator-for-ly")
           (sha256
            (base32
             "00vd236x987b9r71mqz877aad9mc4l3hh8fvzwjirsjakj01szkf"))))
       ("ctypes"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/cylgom/ctypes")
                  (commit "5dd979d3644ab0c85ca14e72b61e6d3d238d432b")))
           (file-name "ctypes-for-ly")
           (sha256
            (base32
             "0faahn2wgckhf3pyn9za594idy9gc5fxdm02ghn6mm3r4fk34xyx"))))
       ("dragonfail"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/cylgom/dragonfail")
                  (commit "6b40d1f8b7f6dda9746e688666af623dfbcceb94")))
           (file-name "dragonfail-for-ly")
           (sha256
            (base32
             "0slsqw6q859vg0h0w92mmjwwh8m65m09h7pq59p90ixrga5q7jch"))))
       ("termbox_next"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/cylgom/termbox_next")
                  (commit "7b85905531bf9e5908c67276dac55d3241361f20")))
           (file-name "termbox_next-for-ly")
           (sha256
            (base32
             "0b43453h3w4h6y1xmswbnl9y0vxilnqvsmgqfbpvca3l79678mfq"))))))
    (inputs
     `(("libxcb" ,libxcb)
       ("linux-pam" ,linux-pam)
       ("ncurses" ,ncurses)))
    (home-page "https://github.com/cylgom/ly")
    (synopsis "TUI display manager")
    (description "Ly is a lightweight TUI (ncurses-like) display manager for
Linux and BSD.  Ly should work with any X desktop environment, and provides
basic wayland support (sway works very well, for example).")
    (license license:wtfpl2)))

(define-public ly-git
  (let ((commit "aa25ede8f95127a4e00df7b2752770722ecf5821")
        (revision "1"))
    (package
      (inherit ly)
      (name "ly-git")
      (version (git-version (package-version ly) revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/cylgom/ly")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0xv2zxp60khqrmy6349qywpp4fnn6dadwlp1pvyisq1a9lxzpsmq")))))))
