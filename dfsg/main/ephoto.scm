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

(define-module (dfsg main ephoto)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages check)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages pkg-config))

(define-public ephoto
  (package
    (name "ephoto")
    (version "1.0-beta2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://www.smhouston.us/stuff/ephoto-"
                            version ".tar.xz"))
        (sha256
         (base32
          "09crwhprylsa03mp8vb14balyh5x0zai283gsr8hnl6zkx2bxxah"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("check" ,check)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)))
    (home-page "http://smhouston.us/ephoto/")
    (synopsis "EFL image viewer/editor/manipulator/slideshow creator")
    (description "Ephoto is an image viewer and editor written using the
Enlightenment Foundation Libraries (EFL).  It focuses on simplicity and ease
of use, while taking advantage of the speed and small footprint the EFL provide.

Ephoto’s features include:
@enumerate
@item Browsing the filesystem and displaying images in an easy to use grid view.
@item Browsing images in a single image view format.
@item Viewing images in a slideshow.
@item Editing your image with features such as cropping, auto enhance,
blurring, sharpening, brightness/contrast/gamma adjustments, hue/saturation/value
adjustments, and color level adjustment.
@item Applying artistic filters to your image such as black and white and old photo.
@item Drag And Drop along with file operations to easy maintain your photo directories.
@end enumerate\n")
    (license (list
               license:bsd-2 ; Ephoto's thumbnailing code
               license:bsd-3))))
