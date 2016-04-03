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

(define-module (packages bambam)
  #:use-module ((guix licenses) #:prefix license:)
  ;#:use-module (guix svn-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl))

(define-public smpeg
  (package
    (name "smpeg")
    (version "0.4.5+cvs20030824")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/s/smpeg/"
                            "smpeg_" version ".orig.tar.gz"))
        ;; Unfortunately the svn-fetch method does not work with this repo
        ;(method svn-fetch)
        ;(uri (svn-reference
        ;       (url "https://svn.icculus.org/smpeg/")
        ;       (revision 408)))
        (sha256
         (base32
           "05hrprv8h0bw8xxzraccq922b4jk04c3zwyk5nhyizfrgmwylxhj"))))
        ;  "0cyl0ww4fjlf289pjxa53q4klyn55ajvkgymw0qrdgp4593raq52"))
        ;(file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (inputs
     `(("sdl" ,sdl)
       ("sdl-mixer" ,sdl-mixer)))
    (home-page "https://icculus.org/smpeg/")
    (synopsis "SDL MPEG Player Library")
    (description "SMPEG (SDL MPEG Player Library) is a free MPEG1 video player
library with sound support.  Video playback is based on the ubiquitous Berkeley
MPEG player, mpeg_play v2.2.  Audio is played through a slightly modified
mpegsound library, part of splay v0.8.2. SMPEG supports MPEG audio (MP3), MPEG-1
video, and MPEG system streams.")
    (license #f)))

(define-public python-pygame
  (package
    (name "python-pygame")
    (version "1.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://pygame.org/ftp/pygame-"
                            version "release.tar.gz"))
        (sha256
         (base32
          "0cyl0ww4fjlf289pjxa53q4klyn55ajvkgymw0qrdgp4593raq52"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (inputs
     `(("freetype" ,freetype)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("python2-numpy" ,python2-numpy)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-ttf" ,sdl-ttf)
       ("smpeg" ,smpeg)))
    (home-page "http://www.pygame.org/")
    (synopsis "SDL bindings for games development in Python")
    (description "A multimedia development kit for Python.  Pygame provides
modules for you to access the video display, play sounds, track time, read the
mouse and joystick, control the CD player, render true type fonts and more.  It
does this using mainly the cross-platform SDL library, a lightweight wrapper to
OS-specific APIs.")
    (license (list license:lgpl2.1
                   license:public-domain)))) ; programs in example directory
