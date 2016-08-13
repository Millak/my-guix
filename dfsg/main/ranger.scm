;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main ranger)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages less)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (gnu packages w3m))

(define-public ranger
  (package
    (name "ranger")
    (version "1.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://ranger.nongnu.org/ranger-" version ".tar.gz"))
        (sha256
         (base32
          "0yaviybviwdvfg2a0pf2kk28g10k245499xmbpqlai7fv91f7xll"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; There aren't any tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((caca    (assoc-ref inputs "libcaca"))
                   (less    (assoc-ref inputs "less"))
                   (poppler (assoc-ref inputs "poppler"))
                   (sudo    (assoc-ref inputs "sudo"))
                   (w3m     (assoc-ref inputs "w3m")))
               (substitute* '("ranger/data/scope.sh" "doc/config/scope.sh")
                            (("img2txt") (string-append caca "/bin/img2txt")))
               (substitute* '("ranger/ext/rifle.py" "ranger/__init__.py" "ranger/core/runner.py" "scripts/rifle")
                            (("less") (string-append less "/bin/less")))
               (substitute* '("ranger/data/scope.sh" "doc/config/scope.sh")
                            (("pdftotext") (string-append poppler "/bin/pdftotext")))
               (substitute* '("ranger/ext/rifle.py" "ranger/core/runner.py" "ranger/config/rifle.conf" "scripts/rifle")
                            (("sudo") (string-append sudo "/bin/sudo")))
               (substitute* "ranger/ext/img_display.py"
                            (("/usr/lib/w3m/w3mimgdisplay") (string-append w3m "/libexec/w3m/w3mimgdisplay")))
               (substitute* '("examples/rc_emacs.conf" "ranger/config/rc.conf" "ranger/config/rifle.conf" "ranger/data/scope.sh" "doc/config/rc.conf" "doc/config/rifle.conf" "doc/config/scope.sh")
                            (("\\ w3m") (string-append w3m "/bin/w3m")))
               ))))))


    ;; TODO: wrap the binary with all the inputs that otherwise would need to be propagated
    (inputs
     `(
       ("file" ,file)
       ("less" ,less)
       ("libcaca" ,libcaca)
       ("poppler" ,poppler)
       ("python-chardet" ,python-chardet)
       ("sudo" ,sudo)
       ("w3m" ,w3m)))
    (home-page "http://ranger.nongnu.org/")
    (synopsis "File manager with an ncurses frontend written in Python")
    (description
     "Ranger is a console file manager that gives you greater flexibility
and a good overview of your files without having to leave your *nix console.  It
visualizes the directory tree in two dimensions: the directory hierarchy on one,
lists of files on the other, with a preview to the right so you know where
you'll be going.
The default keys are similar to those of Vim, Emacs and Midnight Commander,
though Ranger is easily controllable with just the arrow keys or the mouse.")
    (license license:agpl3+)))
