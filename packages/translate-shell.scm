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

(define-module (packages translate-shell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages mp3))

(define-public translate-shell
  (package
    (name "translate-shell")
    (version "0.9.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/soimort/" name "/archive/v"
                            version ".tar.gz"))
        (sha256
         (base32
          "0fijjgcgihn0dlvp95vfhs9w3g2fhpzs6dn7x6l82v9gh2975psd"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)) ; no configure phase
       #:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f))
    (propagated-inputs
     `(("curl" ,curl)
       ("fribidi" ,fribidi)
       ("gawk" ,gawk)
       ("mpg123" ,mpg123)))
    (home-page "https://github.com/soimort/translate-shell")
    (synopsis "Cli translator using Google translate")
    (description "Translate-shell is a simple command line interface to
@url{http://translate.google.com} which allows you to translate strings in your
terminal.")
    (license license:public-domain)))
