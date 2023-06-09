;;; Copyright © 2017, 2019, 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main hspell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages perl))

;; This package probably works as intended, and passes all the tests, but I
;; haven't been able to verify it outside of the 'check phase since I have no
;; ISO-8859-8 files.
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
     '(#:configure-flags (list "--enable-fatverb"
                               "--enable-linginfo"
                               "--enable-shared"
                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
       #:parallel-build? #f     ; Race condition at 24 cores
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-aspell-invocations
           ;; he.dat is in aspell-dict-he, not aspell itself.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("--lang=he")
                (string-append "--local-data-dir="
                               (dirname (search-input-file
                                          inputs "/lib/aspell/he.dat"))
                               " --lang=he")))
             (substitute* "test/test1"
               (("--dict-dir")
                (string-append "--local-data-dir="
                               (dirname (search-input-file
                                          inputs "/lib/aspell/he.dat"))
                               " --dict-dir")))))
         (add-after 'unpack 'set-perl-path
           (lambda _
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":" (getenv "PERL5LIB")))))
         ;; This way the binaries can find their libraries.
         ;; If we didn't build the shared library we wouldn't need this.
         (delete 'check)
         (add-after 'install 'check-after-install
           (lambda* args
             (apply (assoc-ref %standard-phases 'check) args))))))
    (native-inputs
     (list aspell aspell-dict-he hunspell))
    (inputs
     (list perl zlib))
    (home-page "http://hspell.ivrix.org.il/")
    (synopsis "Hebrew linguistic tool")
    (description
     "Hspell's primary goal is to create a free Hebrew spell-checker.  In
addition to a spell-checker, the project also produced a Hebrew morphological
analyzer, which for every valid Hebrew word lists all of its possible readings
(valid combinations of prefix and inflected base word), and for each reading
describe its part-of-speech, gender, tense, and other attributes.")
    (license license:agpl3)))
