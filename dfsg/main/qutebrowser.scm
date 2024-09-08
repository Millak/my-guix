;;; Copyright © 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main qutebrowser)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xdisorg)
  #:use-module (dfsg main adblock))

(define qtwebengine-with-widevine
  (package/inherit qtwebengine-5
    (arguments
     (substitute-keyword-arguments (package-arguments qtwebengine-5)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure 'enable-widevine-support
              (lambda _
                (substitute* "src/buildtools/config/common.pri"
                  (("enable_widevine=false")
                   "enable_widevine=true"))))
            (replace 'configure
              (lambda _
                ;; Valid QT_BUILD_PARTS variables are:
                ;; libs tools tests examples demos docs translations
                (invoke "qmake" "QT_BUILD_PARTS = libs tools" "--"
                        "--webengine-printing-and-pdf=no"
                        "--webengine-ffmpeg=system"
                        ;; FIXME: Building qtwebengine-5 5.12.2 with
                        ;; icu4c >= 68 fails.
                        ;;"--webengine-icu=system"
                        "--webengine-pepper-plugins=yes"
                        "--webengine-proprietary-codecs=yes")))))))))

(define-public qutebrowser-with-adblock
  (package/inherit qutebrowser
    (name "qutebrowser-with-adblock")
    (inputs (modify-inputs (package-inputs qutebrowser)
                           (prepend python-adblock
                                    rofi-wayland
                                    ;; Can only add 1 in the manifest
                                    qtwayland)))
    (arguments
     (substitute-keyword-arguments (package-arguments qutebrowser)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'wrap-qt-process-path 'wrap-qutebrowser-binary
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (wrap-program (search-input-file outputs "bin/qutebrowser")
                 `("PATH" prefix
                   (,(dirname (search-input-file inputs "bin/qmake6"))
                    ,(dirname (search-input-file inputs "bin/rofi"))))
                 `("QT_PLUGIN_PATH" prefix
                   (,(dirname
                       (search-input-directory
                         inputs "lib/qt6/plugins/wayland-decoration-client")))))))))))))

(define-public qutebrowser-with-widevine
  (package/inherit qutebrowser
    (name "qutebrowser-with-widevine")
    (inputs
     (modify-inputs (package-inputs qutebrowser)
                    (replace "qtwebengine" qtwebengine-with-widevine)))))
