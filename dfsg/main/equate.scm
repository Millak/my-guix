;;; Copyright © 2016, 2017, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main equate)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages pkg-config))

(define-public equate
  (let ((commit "2afdce05eba77d454774fcf57dba38dc4baf3f0a")
        (revision "2"))
    (package
      (name "equate")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.enlightenment.org/apps/equate")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "13958n8gaphb1bvvrvc74drw3wz46cpzvk4qasiyrziihgrj918d"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("efl" ,efl)))
      (home-page "https://www.enlightenment.org")
      (synopsis "Elementary based calculator")
      (description "Elementary based calculator")
      (license license:bsd-2))))
