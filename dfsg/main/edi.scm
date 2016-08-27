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
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages pkg-config))

(define-public edi
  (package
    (name "edi")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (list
               (string-append "https://download.enlightenment.org/rel/apps/edi"
                              "/edi-" version ".tar.bz2")
               (string-append "https://github.com/ajwillia-ms/edi/releases/"
                              "download/v" version "/edi-" version ".tar.bz2")))
        (sha256
         (base32
          "02d8hplcviayri8fxws56n362k6zqsf62v8pbn5sbgwrmkqwybhc"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("efl" ,efl)))
    (home-page "https://www.enlightenment.org/about-edi")
    (synopsis "Development environment using the EFL")
    (description "EDI is a development environment designed for and built using
the EFL.  It's aim is to create a new, native development environment for Linux
that trys to lower the barrier to getting involved in Enlightenment development
and in creating apps based on the EFL suite.")
    (license license:gpl2)))
