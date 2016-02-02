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

(define-module (efraim packages vim)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xorg))

(define-public vim-custom
  (package (inherit vim)
    (name "vim-custom")
    (arguments
     `(#:configure-flags '("--enable-python3interp"
                           "--enable-pythoninterp"
                           "--enable-perlinterp=dynamic"
                           "--enable-rubyinterp=dynamic"
                           "--enable-tclinterp=dynamic"
                           ;;"--enable-luainterp=dynamic"
                           "--enable-multibyte"
                           "--enable-gui=gtk2"
                           "--enable-gtk2-check"
                           "--enable-fontset"
                           "--disable-selinux")
       ,@(package-arguments vim)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("python" ,python)
       ("python" ,python-2)
       ,@(package-inputs vim)))))
