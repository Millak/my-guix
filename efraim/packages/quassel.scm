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

(define-module (efraim packages quassel)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

(define-public oxygen-icons
  (package
    (name "oxygen-icons")
    (version "4.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://download.kde.org/stable/" version
                            "/src/" name "-" version ".tar.xz"))
        (sha256
         (base32
          "1mz73f54qh2vd8ibp60f6fjflrprz0lvqfkgh805l7wfhrv4ckbz"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no test target
    (home-page "http://www.kde.org/")
    (synopsis "Oxygen icon theme for the KDE desktop")
    (description "Oxygen icon theme for the KDE desktop")
    (license license:lgpl3+)))

(define-public quassel
  (package
    (name "quassel")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://quassel-irc.org/pub/quassel-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "0d6lwf6qblj1ia5j9mjy112zrmpbbg9mmxgscbgxiqychldyjgjd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '(;;"-DWANT_QTCLIENT=OFF" ;; These three are not
                           ;;"-DWANT_CORE=OFF" ;; mutually exclusive
                           ;;"-DWANT_MONO=ON"
                           "-DUSE_QT5=ON" ;; default is qt4
                           "-DWITH_KDE=OFF" ;; no to integration
                           "-DWITH_OXYGEN=ON" ;; on=embed icons
                           "-DWITH_WEBKIT=ON") ;; wants qtwebkit, in qt5
       #:tests? #f)) ;; no test target
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("oxygen-icons" ,oxygen-icons)
       ("qt", qt)
       ("zlib" ,zlib)))
    (home-page "http://quassel-irc.org/")
    (synopsis "distributed IRC client")
    (description "Quassel is a modern, cross-platform, distributed IRC client,
meaning that one or more clients can attach to and detach from the central core.
It's much like the popular combination of screen and a text-based IRC client
such as WeeChat or irssi, but graphical.")
    (license (list license:gpl2 license:gpl3)))) ;; dual licensed
