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

(define-module (dfsg main kdeconnect)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public kdeconnect
  (package
    (name "kdeconnect")
    (version "1.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/kdeconnect/"
                            version "/src/kdeconnect-kde-"
                            version ".tar.xz"))
        (sha256
         (base32
          "1vac0mw1myrswr61adv7lgif0c4wzw5wnsj0sqxj6msp4l4pfgsg"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_TESTING=ON")
       #:tests? #f ; tests fail hard in our build environment
       #:modules ((guix build cmake-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%cmake-build-system-modules
                            (guix build qt-utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "QT_PLUGIN_PATH"
                       (string-append out "/lib/qt5/plugins"
                                      ":" (getenv "QT_PLUGIN_PATH")))
               (wrap-qt-program out "../lib/libexec/kdeconnectd")
               (wrap-qt-program out "kdeconnect-cli")
               (wrap-qt-program out "kdeconnect-handler")
               (wrap-qt-program out "kdeconnect-indicator"))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("libxtst" ,libxtst)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("kcmutils" ,kcmutils)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("knotifications" ,knotifications)
       ("kwayland" ,kwayland)
       ("libfakekey" ,libfakekey)
       ("qca" ,qca)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://community.kde.org/KDEConnect")
    (synopsis "Enable your devices to communicate with each other")
    (description "KDE Connect is a project that enables all your devices to
communicate with each other.  Here's a few things KDE Connect can do:
@enumerate
@item Receive your phone notifications on your desktop computer and reply to messages
@item Control music playing on your desktop from your phone
@item Use your phone as a remote control for your desktop
@item Run predefined commands on your PC from connected devices
@item Check your phones battery level from the desktop
@item Ring your phone to help finding it
@item Share files and links between devices
@item Browse your phone from the desktop
@item Control the desktop's volume from the phone
@end enumerate")
    (license (list license:gpl2 license:gpl3))))
