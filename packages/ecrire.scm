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

(define-module (packages ecrire)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config))

(define-public ecrire
  (package
    (name "ecrire")
    (version "20151008")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.enlightenment.org/apps/ecrire.git/")
               (commit "a7da0abdb3eef334ac77b61a8320172531518818")))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "0f7phsdkxm26nlz6lm4ghski0gjrn2lhbnbjcql4phicdb8p00vl"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no tests
    (native-inputs
     `(("gettext" ,gnu-gettext)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)
       ("elementary" ,elementary)))
    (home-page "https://www.enlightenment.org")
    (synopsis "EFL simple text editor")
    (description "EFL simple text editor")
    (license license:gpl3+)))
