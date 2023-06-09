;;; Copyright © 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip epr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python))

(define-public epr
  (package
    (name "epr")
    (version "2.4.13")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "epr-reader" version))
        (sha256
         (base32 "04h2jnknk3w8z5qyj52z83mjjqvqg23cg60jmbvcn1z3af03mz79"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; no tests.
    (home-page "https://github.com/wustho/epr")
    (synopsis "CLI Epub Reader")
    (description
     "Terminal/CLI Epub reader with features:
@enumerate
@item Remembers last read file (just run epr without any argument)
@item Remembers last reading state for each file (per file saved state written
to @code{$HOME/.config/epr/config} or @code{$HOME/.epr} respectively depending
on availability)
@item Adjustable text area width
@item Adaptive to terminal resize
@item Supports EPUB3 (no audio support)
@item Secondary vim-like bindings
@item Supports opening images
@end enumerate")
    (license license:expat)))
