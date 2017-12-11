;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip dpkg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config))

(define-public dpkg
  (package
    (name "dpkg")
    (version "1.18.24")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/d/dpkg/"
                            "dpkg_" version ".tar.xz"))
        (sha256
         (base32
          "1d6p22vk1b9v16q96mwaz9w2xr4ly28yamkh49md9gq67qfhhlyq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("perl-io-string" ,perl-io-string)))
    (inputs
     `(("ncurses" ,ncurses)
       ("perl" ,perl)))
    (home-page "https://wiki.debian.org/Teams/Dpkg")
    (synopsis "Debian package management system")
    (description "This package provides the low-level infrastructure for
handling the installation and removal of Debian software packages.")
    (license license:gpl2+)))
