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

(define-module (dfsg main edi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config))

(define-public edi
  (package
    (name "edi")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ajwillia-ms/edi/releases/"
                            "download/v" version "/edi-" version ".tar.bz2"))
        (sha256
         (base32
          "0qczz5psryxasphg5km95845h510237rf0k1dy8f0dad52ii90j1"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--with-tests=coverage")))
    (native-inputs
     `(("check" ,check)
       ("lcov" ,lcov)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("clang" ,clang)
       ("efl" ,efl)))
    (home-page "https://www.enlightenment.org/about-edi")
    (synopsis "Development environment for Enlightenment")
    (description "EDI is a development environment designed for and built using
the EFL.  It's aim is to create a new, native development environment for Linux
that tries to lower the barrier to getting involved in Enlightenment development
and in creating applications based on the Enlightenment Foundation Library suite.")
    (license (list license:public-domain ; data/extra/skeleton
                   license:gpl2))))      ; edi
