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

(define-module (dfsg main rockchip)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages flashing-tools))

(define-public rkdeveloptool
  (let ((commit "6e92ebcf8b1812da02663494a68972f956e490d3")
        (revision "1"))
    (package
      (inherit rkflashtool)
      (name "rkdeveloptool")
      (version (git-version "1.3" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/rockchip-linux/rkdeveloptool.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0zwrkqfxd671iy69v3q0844gfdpm1yk51i9qh2rqc969bd8glxga"))))
      (arguments
       '(#:tests? #f)) ; no tests
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ,@(package-native-inputs rkflashtool)))
      (home-page "https://github.com/linux-rockchip/rkflashtool")
      (license license:gpl2+))))
