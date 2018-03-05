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

(define-module (wip kdenlive)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  )

(define-public kdenlive
  (package
    (name "kdenlive")
    (version "17.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/applications/" version
                            "/src/kdenlive-" version ".tar.xz"))
        (sha256
         (base32
          "1jn8bbsdishccdp7lqqyr9y7wcw7rq4gsisp3cjkdbzg44ahjmnp"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Ensure that icons are found at runtime.
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/kdenlive")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins/"))
                         '("qtbase" "qtsvg"))))
               #t))))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("ki18n" ,ki18n)
       ("pkg-config" ,pkg-config)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("karchive" ,karchive)
       ("kbookmarks" ,kbookmarks)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kfilemetadata" ,kfilemetadata)
       ("kguiaddons" ,kguiaddons)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("mlt" ,mlt)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("qtsvg" ,qtsvg)
       ("qtwebkit" ,qtwebkit)
       ("v4l-utils" ,v4l-utils)))
    (propagated-inputs
     `(("breeze-icons" ,breeze-icons) ; icons are good
       ("qtdeclarative" ,qtdeclarative) ; needed to launch
       ("qtquickcontrols" ,qtquickcontrols))) ; needed to launch
    (home-page "https://kdenlive.org/")
    (synopsis "non-linear video editor")
    (description "Kdenlive is a powerful non-linear multi-track video editing
suite, which supports DV, HDV and many more formats.")
    (license license:gpl2+)))

(define-public plasma-workspace
  (package
    (name "plasma-workspace")
    (version "5.11.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/plasma/" version
                           "/plasma-workspace-" version ".tar.xz"))
       (sha256
        (base32
         "1ipklc6v2ml095sy80rgq4123vkk3famjwf8s3rgkk172s76qm98"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(
       ("baloo" ,baloo)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcmutils" ,kcmutils)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kdelibs4support" ,kdelibs4support)
       ("kdesu" ,kdesu)
       ("kdoctools" ,kdoctools)
       ("kglobalaccel" ,kglobalaccel)
       ("kidletime" ,kidletime)
       ("knewstuff" ,knewstuff)
       ("knotifyconfig" ,knotifyconfig)
       ("kpackage" ,kpackage)
       ("krunner" ,krunner)
       ("ktexteditor" ,ktexteditor)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwayland" ,kwayland)
       ("libksysguard" ,libksysguard)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("zlib" ,zlib)
       ))
    (home-page #f)
    (synopsis "")
    (description "")
    (license #f)
    ))
