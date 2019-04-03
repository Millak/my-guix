;;; Copyright © 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main mpv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages video)
  #:use-module (srfi srfi-1))

(define-public my-mpv
  (package
    (inherit mpv)
    (name "my-mpv")
    (inputs
     `(,@(fold alist-delete (package-inputs mpv)
               '("jack" "lua"))))))

(define-public mpv-mpris
  (package
    (name "mpv-mpris")
    (version "0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hoyon/mpv-mpris")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "06hq3j1jjlaaz9ss5l7illxz8vm5bng86jl24kawglwkqayhdnjx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "mpris.so" (string-append out "/lib")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("mpv" ,my-mpv)))
    (home-page "https://github.com/hoyon/mpv-mpris")
    (synopsis "MPRIS plugin for mpv")
    (description "MPRIS plugin for mpv written in C.  Requires mpv to be built
with @code{--enable-cplugins} (default as of mpv 0.26).  Implements
@code{org.mpris.MediaPlayer2} and @code{org.mpris.MediaPlayer2.Player} D-Bus
interfaces.

To load this plugin, specify the following option when starting mpv:
@code{--script $GUIX_PROFILE/lib/mpris.so} or link it into
$HOME/.config/mpv/scripts")
    (license license:expat)))
