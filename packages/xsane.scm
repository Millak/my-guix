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

(define-module (packages xsane)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages scanner))

(define-public xsane
  (package
    (name "xsane")
    (version "0.999")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://www.xsane.org/download/xsane-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0jrb918sfb9jw3vmrz0z7np4q55hgsqqffpixs0ir5nwcwzd50jp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gimp" ,gimp)
       ("gtk+-2" ,gtk+-2)
       ("libjpeg" ,libjpeg)
       ("libtiff" ,libtiff)
       ("sane" ,sane-backends)))
    (home-page "http://www.xsane.org/")
    (synopsis "Graphical frontend for SANE")
    (description "Xsane can be run as a stand-alone program or through the GIMP
image manipulation program.  In stand-alone mode, xsane can save an image to a
file in a variety of image formats, serve as a frontend to a fax program, or
send an image to a printer.")
    (license license:gpl2+)))
