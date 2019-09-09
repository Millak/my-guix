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

(define-module (dfsg main ktouch)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public ktouch
  (package
    (name "ktouch")
    (version "19.08.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/applications/"
                            version "/src/ktouch-" version ".tar.xz"))
        (sha256
         (base32
          "19rdk94pls75hdvx11hnfk3qpm6l28p9q45q5f04sknxagrfaznr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%cmake-build-system-modules
                            (guix build qt-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-makefiles
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((qtdec (assoc-ref inputs "qtdeclarative")))
               (substitute* '("src/CMakeFiles/ktouch_autogen.dir/build.make"
                              "src/CMakeFiles/ktouch.dir/build.make")
                 (("/gnu/store/.*qmlcachegen")
                  (string-append qtdec "/bin/qmlcachegen"))))
             #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-qt-program out "ktouch"))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("kqtquickcharts" ,kqtquickcharts)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libxcb" ,libxcb)
       ("libxkbfile" ,libxkbfile)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtx11extras" ,qtx11extras)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://edu.kde.org/ktouch/")
    (synopsis "Touch typing tutor")
    (description
     "KTouch is an aid for learning how to type with speed and accuracy.  It
provides a sample text to type and indicates which fingers should be used for
each key.  A collection of lessons are included for a wide range of different
languages and keyboard layouts, and typing statistics are used to dynamically
adjust the level of difficulty.")
    (license license:gpl2)))

(define-public kqtquickcharts
  (package
    (name "kqtquickcharts")
    (version "19.08.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/applications/"
                            version "/src/kqtquickcharts-" version ".tar.xz"))
        (sha256
         (base32
          "1j3rivvh4sa94lsd0hi4xfvcikl05zrqd7634wxyaxs718ais6dg"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://phabricator.kde.org/source/kqtquickcharts/")
    (synopsis "Interactive charts for Qt Quick")
    (description
     "A QtQuick plugin to render beautiful and interactive charts.")
    (license license:lgpl2.1+)))
