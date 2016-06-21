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

(define-module (packages ephoto)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config))

(define-public ephoto
  (package
    (name "ephoto")
    (version "20160621")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.enlightenment.org/apps/ephoto.git/")
               (commit "ab8edacd0d9cf26ac8e6df524e6c40462308b033")))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "1m4yg5f5sl56rhaybx3xhvg848r0hfh108wb8qjaxflbl9f8rx6c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _ (zero? (and (setenv "NOCONFIGURE" "TRUE")
                                 (system* "sh" "autogen.sh"))))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("check" ,check)
       ("gettext" ,gnu-gettext)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)
       ("elementary" ,elementary)))
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
    (license (list license:bsd-2 license:bsd-3))))
