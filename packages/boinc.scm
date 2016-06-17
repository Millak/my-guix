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

(define-module (packages boinc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls))

(define-public boinc
  (package
    (name "boinc")
    (version "7.6.33")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/BOINC/boinc/archive/"
                            "client_release/" (version-major+minor version)
                            "/" version ".tar.gz"))
        (sha256
         (base32
          "0vdak578bay8knfjmw2rc723h3zksxxg9p8xqq53w0amjsdw5cf4"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autosetup
           (lambda _
             (zero? (system* "sh" "_autosetup"))))
         (add-before 'install 'fix-install-path
           (lambda _
             (substitute* "client/scripts/Makefile.am"
                          (("\\$\\(DESTDIR\\)")
                           (assoc-ref %outputs "out"))))))
       #:configure-flags (list "--disable-server"
                               "--disable-manager")))
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)
       ("m4" ,m4)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2) ; for the tests
       ("zlib" ,zlib)))
    (inputs
     `(("curl" ,curl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("openssl" ,openssl)))
    (home-page "http://boinc.berkeley.edu/")
    (synopsis "Software for distributed and grid computing")
    (description "Use the idle time on your computer to cure diseases, study
global warming, discover pulsars, and do many other types of scientific
research.")
    (license license:lgpl3+)))
