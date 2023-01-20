;;; Copyright © 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages hunspell)
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
     '(#:configure-flags (list "--enable-fatverb"
                           "--enable-linginfo"
                           "--enable-shared"
                           "--enable-test"
                           (string-append "LDFLAGS=-Wl,-rpath="
                                          (assoc-ref %outputs "out")
                                          "/lib"))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-aspell-test
           ;; he.dat is not in the aspell directory
           (lambda _
             (substitute* "Makefile.in"
               (("he.rws ") " "))
             (substitute* "test/test1"
               (("test_all\\ aspell") "#test_all aspell"))
             #t))
         (add-after 'unpack 'set-perl-path
           (lambda _
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":" (getenv "PERL5LIB")))
           #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include"))
                    (share (string-append out "/share"))
                    (man1 (string-append share "/man1"))
                    (man3 (string-append share "/man3")))
               (install-file "hspell" bin)
               (install-file "multispell" bin)
               (symlink "hspell" (string-append bin "/hspell-i"))
               (install-file "libhspell.a" lib)
               (for-each
                 (lambda (file)
                   (install-file file share))
                 (find-files "." "^hebrew.wgz"))
               (install-file "hspell.1" man1)
               (install-file "hspell.3" man3)
               (install-file "linginfo.h" inc)
               (install-file "hspell.h" inc)
               (when (file-exists? "libhspell.so.0")
                 (install-file "libhspell.so.0" lib)
                 (symlink "libhspell.so.0" (string-append lib "/libhspell.so"))))
         ;    #t))
         ;(add-after 'install 'wrap-binaries
         ;  (lambda* (#:key outputs #:allow-other-keys)
         ;    (let* ((out (assoc-ref outputs "out"))
         ;           (bin (string-append out "/bin"))
         ;           (lib (string-append out "/lib")))
         ;      (for-each
         ;        (lambda (file)
         ;          (wrap-program file
         ;            `("PATH" ":" prefix (,lib))))
         ;        (find-files bin ".*")))
             #t)))))
    (native-inputs
     `(("aspell" ,aspell)
       ("aspell-dict-he" ,aspell-dict-he)
       ("hunspell" ,hunspell)))
    (inputs
     `(("perl" ,perl)
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
