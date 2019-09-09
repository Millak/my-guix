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

(define-module (dfsg main klavaro)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  )

(define-public klavaro
  (package
    (name "klavaro")
    (version "3.09")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/klavaro/klavaro-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "12gml7h45b1w9s318h0d5wxw92h7pgajn2kh57j0ak9saq0yb0wr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("curl" ,curl)
       ("gtk+" ,gtk+)
       ("pango" ,pango)))
    (home-page "https://klavaro.sourceforge.io/en/index.html")
    (synopsis "Touch typing tutor")
    (description
     "Klavaro is a simple tutor to teach correct typing, almost independently of
language and very flexible regarding to new or unknown keyboard layouts.")
    (license license:gpl3+)))
