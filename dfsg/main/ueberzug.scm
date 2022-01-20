;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main ueberzug)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg))


(define-public ueberzug
  (package
    (name "ueberzug")
    (version "18.1.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ueberzug" version))
              (sha256
               (base32
                "1hxd45dnwa9yv908acarr98n2drmar66wzq9z2qd3irj24srzr3w"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))            ; No tests.
    (inputs
     (list libx11
           libxext
           python-attrs
           python-docopt
           python-pillow
           python-xlib))
    (home-page "https://github.com/seebye/ueberzug")
    (synopsis "Command line utility to display images")
    (description
     "@code{ueberzug} is a command line util which allows one to display images
in combination with X11.")
    (license license:gpl3)))
