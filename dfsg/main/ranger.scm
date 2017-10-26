;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages python))

(define-public ranger
  (package
    (name "ranger")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://nongnu.org/ranger/ranger-" version ".tar.gz"))
        (sha256
         (base32
          "1d11qw0mr9aj22a7nhr6p2c3yzf359xbffmjsjblq44bjpwzjcql"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; There aren't any tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-inputs
           (lambda _
             (substitute* '("doc/config/scope.sh"
                            "ranger/data/scope.sh")
               (("/bin/") ""))
             #t)))))
    (inputs `(("python-chardet" ,python-chardet)))
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
