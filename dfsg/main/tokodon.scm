;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main tokodon)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages qt))

(define-public tokodon
  (package
    (name "tokodon")
    (version "23.04.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://invent.kde.org/network/tokodon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06l2rhwinkjh9im83n3rn3124dg1ywm7j4risjhngpyl90dllh3y"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'pre-check
                    (lambda _
                      (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs (list extra-cmake-modules ki18n qtdeclarative-5))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kdbusaddons
           kio
           kirigami
           kirigami-addons
           knotifications
           qqc2-desktop-style
           qtbase-5
           qtgraphicaleffects   ; Needed at runtime.
           qtmultimedia-5
           qtsvg-5
           qtkeychain
           qtquickcontrols-5    ; QtQuick.Dialogs
           qtquickcontrols2-5   ; Needed at runtime.
           qtwebsockets-5
           sonnet))             ; Needed at runtime.
    (home-page "https://apps.kde.org/tokodon/")
    (synopsis "KDE Mastodon client")
    (description "Tokodon is a Mastodon Client.  It allows you to interact with
the Fediverse community.")
    (license license:gpl3)))
