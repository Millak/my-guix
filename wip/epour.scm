;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib) ; intltool
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (srfi srfi-1))

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
     `(#:tests? #f   ; no test target
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "python" "setup.py" "install" (string-append "--prefix=" out))
               #t))))))
    (native-inputs `(("intltool" ,intltool)))
    (propagated-inputs
     `(("libtorrent-rasterbar-local" ,libtorrent-rasterbar-local)
       ("python-dbus" ,python-dbus)
       ("python-distutils-extra" ,python-distutils-extra)
       ("python-efl" ,python-efl)
       ;("python-parse" ,python-parse)
       ("python-pyxdg" ,python-pyxdg)
       ;("python-urllib3" ,python-urllib3)
       ))
    (home-page "https://www.enlightenment.org")
    (synopsis "EFL Bittorrent client")
    (description "Epour is a BitTorrent client based on the @dfn{Enlightenment
Foundation Libraries} (EFL) and rb-libtorrent.")
    (license license:gpl3+)))

(define python-parse
  (package
    (name "python-parse")
    (version "1.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "parse" version))
        (sha256
         (base32
          "0dvli4vcvzkp4zf7pqy70xs2y8mwnyi6m7rg6hm07k8jla709mlx"))))
    (build-system python-build-system)
    (home-page "https://github.com/r1chardj0n3s/parse")
    (synopsis "parse() is the opposite of format()")
    (description
     "Parse strings using a specification based on the Python format() syntax.")
    (license license:expat)))

(define libtorrent-rasterbar-local
  (package
    (inherit libtorrent-rasterbar)
    (inputs
     `(("boost" ,boost-local)
       ,@(alist-delete "boost"
                        (package-inputs libtorrent-rasterbar))))
    (native-inputs
     `(("python" ,python)
       ,@(alist-delete "python-2"
                        (package-native-inputs libtorrent-rasterbar))))))

(define boost-local
  (package
    (inherit boost)
    (native-inputs
     `(("python" ,python)
       ,@(alist-delete "python-2"
                        (package-native-inputs boost))))))
