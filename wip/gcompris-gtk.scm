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

(define-module (wip gcompris-gtk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages games)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages xml))

(define-public gcompris-gtk
  (package
    (name "gcompris-gtk")
    (version "15.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://gcompris.net/download/gtk/src/gcompris-" version ".tar.bz2"))
        (sha256
         (base32
          "0f7wa27vvpn9ansp2aald1pajqlx5d51mvj07ba503yvl7i77fka"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-sdlmixer" ; config wants gstreamer-0.10
                           "--enable-py-build-only"))) ; python can't find python2-pygtk
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnu-chess" ,chess)
       ("glib" ,glib "bin")
       ("gtk+-2" ,gtk+-2)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("python-2" ,python-2)
       ("python2-pycairo" ,python2-pycairo)
       ("python2-pygtk" ,python2-pygtk)
       ("python2-pysqlite" ,python2-pysqlite)
       ("sdl-mixer" ,sdl-mixer)
       ("sqlite" ,sqlite)))
    (home-page "http://gcompris.net/index-en.html")
    (synopsis "Educational games for small children")
    (description
     "A large collection of educational games for small children, designed to be
a unified interface to integrate more educational games.  Language-oriented
games contain vocabulary, sounds, and voices for many different languages.
@enumerate
@item learning how to use a mouse and keyboard
@item learning simple arithmetic
@item learning how to read an analog clock
@item recognize letters after hearing their names
@item reading practice
@item small games (memory games, jigsaw puzzles, ...)
@end enumerate\n")
    (license license:gpl3+)))
