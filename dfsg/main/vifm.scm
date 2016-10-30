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

(define-module (dfsg main vifm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xorg))

(define-public vifm
  (package
    (name "vifm")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/vifm/vifm/vifm-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "07r15kq7kjl3a41sd11ncpsii866xxps4f90zh3lv8jqcrv6silb"))))
    (build-system gnu-build-system)
    (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'patch-source-shebangs 'patch-test-shebangs
          (lambda _
            (substitute* (find-files "tests" "\\.c$")
                         (("/bin/sh") (which "sh")))
            #t)))))
    (native-inputs
     `(("groff" ,groff) ; for the documentation
       ("perl" ,perl)))
    (inputs
     `(("libx11" ,libx11)
       ("ncurses" ,ncurses)))
    (home-page "http://vifm.info/")
    (synopsis "Flexible vi-like file manager using ncurses")
    (description "Vifm is a file manager providing a @code{vi}-like usage
experience.  It has similar keybindings and modes (e.g. normal, command line,
visual).  The interface uses ncurses, thus vifm can be used in text-only
environments.  It supports a wide range of features, some of which are known
from the @code{vi}-editor:
@enumerate
@item utf8 support
@item user mappings (almost like in @code{vi})
@item ranges in command
@item line commands
@item user defined commands (with support for ranges)
@item registers
@item operation undoing/redoing
@item fuse file systems support
@item trash
@item multiple files renaming
@item support of filename modifiers
@item colorschemes support
@item file name color according to file type
@item path specific colorscheme customization
@item bookmarks
@item operation backgrounding
@item customizable file viewers
@item handy @code{less}-like preview mode
@item filtering out and searching for files using regular expressions
@item one or two panes view
@end enumerate
With the package comes a plugin to use vifm as a vim file selector.")
    (license license:gpl2+)))
