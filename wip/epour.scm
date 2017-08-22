;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip epour)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib) ; intltool
  #:use-module (gnu packages python))

(define-public epour
  (package
    (name "epour")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/apps/epour"
                            "/epour-" version ".tar.xz"))
        (sha256
         (base32
          "0g9f9p01hsq6dcf4cs1pwq95g6fpkyjgwqlvdjk1km1i5gj5ygqw"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; no test target
    (native-inputs `(("intltool" ,intltool)))
    (inputs
     `(("libtorrent" ,libtorrent)
       ("python-dbus" ,python-dbus)
       ("python-distutils-extra" ,python-distutils-extra)
       ("python-efl" ,python-efl)
       ("python-pyxdg" ,python-pyxdg)))
    (home-page "https://www.enlightenment.org")
    (synopsis "EFL Bittorrent client")
    (description "EFL Bittorrent client")
    (license license:gpl3+)))
