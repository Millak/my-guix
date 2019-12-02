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

(define-module (dfsg main fonts)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system font)
  #:use-module (gnu packages compression))

(define-public font-opendyslexic
  (package
    (name "font-opendyslexic")
    (version "0.91.12")
    (source
      (origin
        (method url-fetch/zipbomb)
        (uri (string-append "https://github.com/antijingoist/opendyslexic/"
                            "releases/download/v" version
                            "/opendyslexic-0.910.12-rc2-2019.10.17.zip"))
        (sha256
         (base32
          "11ml7v4iyf3hr0fbnkwz8afb8vi58wbcfnmn4gyvrwh9jk5pybdr"))))
    (build-system font-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (home-page "https://opendyslexic.org/")
    (synopsis "Typeface for Dyslexia")
    (description "OpenDyslexic is a typeface designed against some common
symptoms of dyslexia.")
    (license license:silofl1.1)))
