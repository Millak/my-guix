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

(define-module (packages vim)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages vim))

(define-public vim-custom
  (package (inherit vim)
    (name "vim-custom")
    (arguments
     `(#:configure-flags (list (string-append "--with-lua-prefix=" (assoc-ref %build-inputs "lua"))
                           "--with-features=huge"
                           "--enable-python3interp=yes"
                           "--enable-pythoninterp=yes"
                           "--enable-perlinterp=yes"
                           "--enable-rubyinterp=yes"
                           "--enable-tclinterp=yes"
                           "--enable-luainterp=yes"
                           "--enable-cscope"
                           "--enable-sniff"
                           "--enable-multibyte"
                           "--disable-selinux")
       ,@(package-arguments vim)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("lua" ,lua)
       ("python" ,python-wrapper)
       ("python" ,python-2)
       ("ruby" ,ruby)
       ("tcl" ,tcl)
       ,@(package-inputs vim)))))
