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

(define-module (dfsg contrib b43-fwcutter)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu))

(define-public b43-fwcutter
  (package
    (name "b43-fwcutter")
    (version "019")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://bues.ch/b43/fwcutter/"
                            "b43-fwcutter-" version ".tar.bz2"))
        (sha256
         (base32
          "1ki1f5fy3yrw843r697f8mqqdz0pbsbqnvg4yzkhibpn1lqqbsnn"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:make-flags
       #~(list (string-append "CC=" #$(cc-for-target))
               (string-append "PREFIX=" #$output))
       #:tests? #f                ; No tests.
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)      ; No configure script
           (replace 'install
             (lambda _
               (install-file "b43-fwcutter" (string-append #$output "/bin"))
               (install-file "b43-fwcutter.1"
                             (string-append #$output "/share/man/man1")))))))
    (home-page "https://wireless.wiki.kernel.org/en/users/Drivers/b43")
    (synopsis "Tool to extract firmware from binary Broadcom 43xx driver files")
    (description "@code{b43-fwcutter} can extract the firmware for your
Broadcom 43xx hardware from different closed source drivers.  The b43 driver
depends on these firmware files and can't work without them.  Currently
b43-fwcutter supports Apple MacOS X, Microsoft Windows and Linux drivers, but
keep in mind that b43-fwcutter doesn't support all driver versions.")
    (license license:bsd-2)))
