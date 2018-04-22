;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip zygrib)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages image)
  #:use-module (gnu packages qt)
  )

(define-public zygrib
  (package
    (name "zygrib")
    (version "8.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://www.zygrib.org/getfile.php?file=zyGrib-" version ".tgz"))
        (file-name (string-append name "-" version ".tgz"))
        (sha256
         (base32
          "1y1hk1mya0gyg4ayj44yjq28zi5lbisbql7ir39sdkklxk5r014w"))
        (patches (search-patches "zygrib-qt5.patch"))
        (modules '((guix build utils)))
        (snippet 
         '(delete-file-recursively "src/qwt-6.1.3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'build-g2clib
           (lambda _
             (substitute* "src/g2clib/makefile"
               (("-m64") "")
               (("-D__64BIT__") "")
               (("/bin/sh") (which "sh")))
             (substitute* "src/g2clib/enc_jpeg2000.c"
               (("image.inmem_=1;") ""))
             (with-directory-excursion "src/g2clib"
               (zero? (system* "make")))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("/usr/bin/qmake") (which "qmake"))
                 )
               (substitute* "src/zyGrib.pro"
                 ((" qwt-6.1.3/src") "")
                 (("-Lqwt-6.1.3/lib/ ") ""))
               (with-directory-excursion "src"
                 (zero? (system* "qmake" "zyGrib.pro"
                        ))))))
         )))
    (native-inputs
     `(
       ("qtbase" ,qtbase)
       ))
    (inputs
     `(
       ("jasper" ,jasper)
       ("libpng" ,libpng)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("qwt" ,qwt)
       ))
    (home-page "http://zygrib.org")
    (synopsis "Weather data visualization")
    (description "")
    (license license:gpl3)))
