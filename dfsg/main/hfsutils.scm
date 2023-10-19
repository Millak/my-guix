;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main hfsutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools))

(define-public hfsutils
  (package
    (name "hfsutils")
    (version "3.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "ftp://ftp.mars.org/pub/hfs/"
                            "hfsutils-" version ".tar.gz"))
        (sha256
         (base32
          "0h4q51bjj5dvsmc2xx1l7ydii9jmfq5y066zkkn21fajsbb257dw"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'bootstrap 'force-bootstrap
             (lambda _
               (delete-file "configure")))
           (add-before 'install 'pre-install
             (lambda _
               (mkdir-p (string-append #$output "/bin"))
               (mkdir-p (string-append #$output "/share/man/man1")))))))
    (native-inputs
     (list autoconf automake))
    (home-page "https://www.mars.org/home/rob/proj/hfs/")
    (synopsis "Tools for reading and writing Macintosh volumes")
    (description "This package contains several command-line utilities for
reading and writing Macintosh HFS-formatted media such as floppy disks,
CD-ROMs, and hard disks.")
    (license license:gpl2+)))
