;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main bidiv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages fribidi))

(define-public bidiv
  (package
    (name "bidiv")
    (version "1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/b/bidiv/bidiv_"
                            version ".orig.tar.gz"))
        (sha256
         (base32
          "05p5m2ihxbmc1qsgs8rjlww08fy9859fhl7xf196p8g5qygqd7cv"))
        (patches (search-patches "bidiv-update-fribidi.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure
         (add-after 'unpack 'misc-fixes
           (lambda _
             (substitute* "bidiv.c"
               (("FriBidiCharType") "FriBidiParType")
               (("&c") "(char *)&c"))
             #t))
         ;; We don't want to use the handwritten makefile
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((fribidi (assoc-ref inputs "fribidi")))
               (invoke "gcc" "-o" "bidiv" "bidiv.c"
                       ;; pkg-config --cflags fribidi
                       (string-append "-I" fribidi "/include/fribidi")
                       ;; pkg-config --libs fribidi
                       (string-append "-L" fribidi "/lib") "-lfribidi"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "bidiv" bin)
               (install-file "bidiv.1" man))
             #t)))
       #:tests? #f)) ; no tests
    (inputs
     `(("fribidi" ,fribidi)))
    (home-page "https://tracker.debian.org/pkg/bidiv")
    (synopsis "BiDi viewer - command-line tool displaying logical Hebrew/Arabic")
    (description "bidiv is a simple utility for converting logical-Hebrew input
to visual-Hebrew output.  This is useful for reading Hebrew mail messages,
viewing Hebrew texts, etc.  It was written for Hebrew but Arabic (or other BiDi
languages) should work equally well.")
    (license license:gpl2+)))
