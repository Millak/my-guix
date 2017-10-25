;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip hspell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages perl))

(define-public hspell
  (package
    (name "hspell")
    (version "1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://hspell.ivrix.org.il/hspell-" version ".tar.gz"))
        (sha256
         (base32
          "18xymabvwr47gi4w2sw1galpvvq2hrjpj4aw45nivlj0hzaza43k"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-fatverb"
                           "--enable-linginfo"
                           "--enable-shared"
                           "--enable-test")))
    (inputs
     `(("aspell" ,aspell)
       ("aspell-dict-he" ,aspell-dict-he)
       ("hunspell" ,hunspell)
       ("perl" ,perl)
       ("zlib" ,zlib)))
    (home-page "http://hspell.ivrix.org.il/")
    (synopsis "Hebrew linguistic tool")
    (description
     "Hspell's primary goal is to create a free Hebrew spell-checker.  In
addition to a spell-checker, the project also produced a Hebrew morphological
analyzer, which for every valid Hebrew word lists all of its possible readings
(valid combinations of prefix and inflected base word), and for each reading
describe its part-of-speech, gender, tense, and other attributes.")
    (license license:agpl3)))
