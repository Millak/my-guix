;;; Copyright © 2024, 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main pgzero)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages music)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sdl))

(define-public python-pgzero
  (package
    (name "python-pgzero")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lordmauve/pgzero")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wsbclznd1rqg0h3xpajqfmd8kwjdx99mbyxv1w23hr5rw5d0xrv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "SDL_VIDEODRIVER" "dummy")
              (setenv "SDL_AUDIODRIVER" "disk"))))))
    (propagated-inputs (list python-numpy python-pygame))
    (native-inputs
     (append
       (list python-setuptools python-pytest)
       ;; Additional programs for the tests
       (if (%current-target-system)
           '()
           (list freetype
                 portmidi
                 sdl2
                 sdl2-image
                 sdl2-mixer
                 sdl2-ttf))))
    (home-page "http://pypi.python.org/pypi/pgzero")
    (synopsis "Zero-boilerplate 2D games framework")
    (description "This package provides a zero-boilerplate 2D games framework
built around pygame.")
    ;; Not including the licenses of the files in the examples directory.
    (license license:lgpl3)))
