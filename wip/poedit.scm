;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip poedit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages wxwidgets)
  )

(define-public poedit
  (package
    (name "poedit")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/vslavik/poedit/releases/"
                            "download/v" version "-oss/poedit-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1rfbncvb33fnkvhjrmbff2i8yphpw712vlh1n8q97mjw4brng0p2"))))
    (build-system gnu-build-system)
    (inputs
     `(
       ("boost" ,boost)
       ("icu4c" ,icu4c)
       ("wxwidgets" ,wxwidgets)
       ))
    (home-page "https://poedit.net")
    (synopsis "Gettext translations editor")
    (description
     "This program is GUI frontend to GNU gettext utilities and catalogs
editor/source code parser.  It helps with translating application into
another language.")
    (license license:expat)))
