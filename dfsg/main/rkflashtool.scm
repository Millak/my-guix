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

(define-module (dfsg main rkflashtool)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config))

(define-public rkflashtool
  (package
    (name "rkflashtool")
    (version "0.0.0-20160221")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/linux-rockchip/rkflashtool.git")
              (commit "9c91cb1fd62e1bb2579d59619e58818f20b5e1c1")))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "1mw3iw8nx7xwcdwf8d03lbd22pxc8iph7nhldx1z1qda809y1zq5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)) ; no configure
       #:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f)) ; no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libusb" ,libusb)))
    (home-page "https://github.com/linux-rockchip/rkflashtool")
    (synopsis "Tools for flashing Rockchip devices")
    (description "Allows flashing of Rockchip based embedded linux devices.
The list of currently supported devices is: RK2818, RK2918, RK2928, RK3026, RK3036, RK3066, RK312X, RK3168, RK3188, RK3288, RK3368.")
    (license license:bsd-2)))
