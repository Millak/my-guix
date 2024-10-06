;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main qt)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages qt)
  #:use-module (srfi srfi-1))

(define-public qtwayland-helper
  (package
    (name "qtwayland-helper")
    (version (package-version qtwayland))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((out (assoc-ref %outputs "out"))
               (qtbase-5    (assoc-ref %build-inputs "qtbase-5"))
               (qtbase      (assoc-ref %build-inputs "qtbase"))
               (qtwayland-5 (assoc-ref %build-inputs "qtwayland-5"))
               (qtwayland   (assoc-ref %build-inputs "qtwayland")))
         (mkdir-p (string-append out "/lib/qt5"))
         (mkdir-p (string-append out "/lib/qt6"))
         (mkdir-p (string-append out "/share"))
         ;; We use copy-recursively instead of union-build because the
         ;; union-build used for profile generation can't add other plugins
         ;; to an existing union-build.
         (copy-recursively (string-append qtbase-5 "/lib/qt5/plugins")
                           (string-append out "/lib/qt5/plugins"))
         (copy-recursively (string-append qtwayland-5 "/lib/qt5/plugins")
                           (string-append out "/lib/qt5/plugins"))
         (copy-recursively (string-append qtbase "/lib/qt6/plugins")
                           (string-append out "/lib/qt6/plugins"))
         (copy-recursively (string-append qtwayland "/lib/qt6/plugins")
                           (string-append out "/lib/qt6/plugins"))
         (copy-recursively (string-append qtbase-5 "/share/doc")
                           (string-append out "/share/doc"))
         (copy-recursively (string-append qtbase "/share/doc")
                           (string-append out "/share/doc"))
         (copy-recursively (string-append qtwayland-5 "/share/doc")
                           (string-append out "/share/doc"))
         (copy-recursively (string-append qtwayland "/share/doc")
                           (string-append out "/share/doc"))
         (copy-recursively (string-append qtwayland-5 "/lib/qt5/qml")
                           (string-append out "/lib/qt5/qml"))
         (copy-recursively (string-append qtwayland "/lib/qt6/qml")
                           (string-append out "/lib/qt6/qml"))
         (delete-file-recursively (string-append out "/share/doc/qt5"))
         (delete-file-recursively (string-append out "/share/doc/qt6"))))))
    (inputs
     `(("qtbase-5" ,qtbase-5)
       ("qtbase" ,qtbase)
       ("qtwayland-5" ,qtwayland-5)
       ("qtwayland" ,qtwayland)))
    (native-search-paths
     (delete-duplicates
       (append (package-native-search-paths qtbase-5)
               (package-native-search-paths qtbase))))
    (home-page (package-home-page qtbase))
    (synopsis "Helper package to display Qt applications in wayland")
    (description "This package provides a helper package which can be used to
enable Qt (Qt5 and Qt6) packages to run under wayland.")
    (license (delete-duplicates
               (append (package-license qtbase-5)
                       (package-license qtbase)
                       (package-license qtwayland-5)
                       (package-license qtwayland))))))
