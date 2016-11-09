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

(define-module (wip bidiv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages pkg-config))

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
          "05p5m2ihxbmc1qsgs8rjlww08fy9859fhl7xf196p8g5qygqd7cv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure
         ;; We don't want to use the handwritten makefile
         (replace 'build
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (zero?
               (apply system* "gcc" "-o" "bidiv" "bidiv.c" make-flags)))))
       #:make-flags (list (string-append "PREFIX=" %output))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
    ;   ))
    ;(inputs
    ; `(
       ("fribidi" ,fribidi)))
    (home-page "https://tracker.debian.org/pkg/bidiv")
    (synopsis "BiDi viewer - command-line tool displaying logical Hebrew/Arabic")
    (description "bidiv is a simple utility for converting logical-Hebrew input
to visual-Hebrew output.  This is useful for reading Hebrew mail messages,
viewing Hebrew texts, etc.  It was written for Hebrew but Arabic (or other BiDi
languages) should work equally well.")
    (license license:gpl2+)))
