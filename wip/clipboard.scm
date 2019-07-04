;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip clipboard)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config))

(define-public clipboard
  (package
    (name "clipboard")
    (version "0.1.0_alpha6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Obsidian-StudiosInc/clipboard")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1yw75p1jwcqikybx71p0lycdhzh1gr1pbqb8wp9wzyif9aplfd95"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags (list (string-append "-Dhomedir-install=" (assoc-ref %outputs "out")))
       ;; This really should be in ~/.e/e/... or %enlightenment/modules/...
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-dir
           (lambda _
             (setenv "HOME" (getenv "TMPDIR"))
             #t)))))
    (native-inputs
     `(
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ))
    (inputs
     `(("efl" ,efl)
       ("enlightenment" ,enlightenment)))
    (home-page "https://github.com/Obsidian-StudiosInc/clipboard")
    (synopsis "Clipboard module for E21+ desktop")
    (description "This is a clipboard module we created for the Bodhi Linux's
e17 fork, Moksha.  Despite being developed for Moksha, it should work on any e17
Desktop meeting the requirements below.

Clipboard currently is a simple, basic features clipboard manager.  It maintains
a history of text copied to the clipboard from which you can choose with a bare
minimum of configuration options.  Our main ambition, aside from learning EFL
and enlightenment module programming, was to make it light on system resources
with no additional dependencies, integrate in a natural way with the e17/Moksha
desktops and be a usable alternative to other clipboard managers (Parcellite,
CopyQ, glipper etc.).")
    (license license:gpl3)))
