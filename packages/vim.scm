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
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public vim-custom
  (package (inherit vim)
    (name "vim-custom")
    (version "7.4.1130")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/vim/vim.git")
               (commit (string-append "v" version))))
        (file-name (string-append "vim-" version "-checkout"))
        (sha256
         (base32
          "1phhc2nrllsav331wxlfngivw8wfmg4qf5z5l4fa2bgd80iakjp5"))))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-lua-prefix="
                            (assoc-ref %build-inputs "lua"))
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
             "--enable-xim"
             "--disable-selinux"
             "--enable-gui")
       ,@(package-arguments vim)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("acl" ,acl)
       ("atk" ,atk)
       ("attr" ,attr)
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gettext" ,gnu-gettext)
       ("glib" ,glib)
       ("gpm" ,gpm)
       ("gtk" ,gtk+-2)
       ("harfbuzz" ,harfbuzz)
       ("libice" ,libice)
       ("libpng" ,libpng)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("libxdmcp" ,libxdmcp)
       ("libxt" ,libxt)
       ("libxpm" ,libxpm)
       ("lua" ,lua)
       ("pango" ,pango)
       ("pixman" ,pixman)
       ("python" ,python-wrapper)
       ("python" ,python-2)
       ("ruby" ,ruby)
       ("tcl" ,tcl)
       ,@(package-inputs vim)))))
