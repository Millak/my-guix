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

(define-module (wip lumina)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public lumina
  (package
    (name "lumina")
    (version "1.1.0-p1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/trueos/lumina/archive/v" version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0vdmqa0wviv9iy33nagmq5zn33fcq904nv72q4czqb3dd88qq5h3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-locations
           (lambda _
             (substitute* "src-qt5/core/libLumina/LuminaOS-Linux.cpp"
               (("/usr/") (assoc-ref %outputs "out")))
             (substitute* "src-qt5/OS-detect.pri"
               (("/usr") (assoc-ref %outputs "out")))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "qmake"
                               (string-append "PREFIX=" out)))))))
       ;#:make-flags (list (string-append "-L" (assoc-ref %outputs "out") "/lib"))
       ))
    (inputs
     `(
       ("libxcb" ,libxcb)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsvg" ,qtsvg)
       ("qttools" ,qttools)
       ("qtx11extras" ,qtx11extras)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-wm" ,xcb-util-wm)
       ))
    (home-page "https://lumina-desktop.org/")
    (synopsis "Lumina Desktop Environment")
    (description
     "The Lumina Desktop Environment is a lightweight system interface that is
designed for use on any Unix-like operating system.  It takes a plugin-based
approach, allowing the entire interface to be assembled/arranged by each
individual user as desired, with a system-wide default layout which can be
setup by the system administrator.  This allows every system (or user session)
to be designed to maximize the individual user's productivity.")
    (license license:bsd-3)))
