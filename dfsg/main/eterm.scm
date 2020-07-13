;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main eterm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages xorg))

(define-public eterm
  (let ((commit "e8fb85b56da21113aaf0f5f7987ae647c4413b6c")
        (revision "1"))
    (package
      (name "eterm")
      (version (git-version "0.9.6" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.enlightenment.org/apps/eterm")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0cc83sh9g0vgmp3cx3xd45dzb80bzrwbvavnf99n0sl1vsnxixd5"))
          (modules '((guix build utils)))
          (snippet
           '(begin
              (substitute* "src/options.c"
                ((".*Built on.*") ""))
              #t))))
      (build-system gnu-build-system)
      (arguments
       '(#:configure-flags '("--disable-static")))
      (native-inputs
       `(("automake" ,automake)
         ("autoconf" ,autoconf)
         ("libtool" ,libtool)))
      (inputs
       `(("efl" ,efl)
         ("imlib2" ,imlib2)
         ("libast" ,libast)
         ("libxext" ,libxext)))
      (home-page "https://sourceforge.net/projects/eterm/")
      (synopsis "Color vt102 terminal emulator using EFL")
      (description "Eterm is a color vt102 terminal emulator with enhanced
graphical capabilities.  Eterm is intended to be a replacement for xterm for
Enlightenment window manager users, but it can also be used as a replacement for
xterm by users without Enlightenment.  Eterm supports various themes and is very
configurable, in keeping with the philosophy of Enlightenment.")
      (license license:bsd-3))))

(define-public libast
  (package
    (name "libast")
    (version "0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://www.eterm.org/download/libast-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1w7bs46r4lykfd83kc3bg9i1rxzzlb4ydk23ikf8mx8avz05q1aj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     `(("libxt" ,libxt)))
    (inputs
     `(("freetype" ,freetype)
       ("imlib2" ,imlib2)
       ("pcre" ,pcre)))
    (home-page "https://sourceforge.net/projects/eterm/")
    (synopsis "Library of Assorted Spiffy Things")
    (description "LibAST is the Library of Assorted Spiffy Things.  The plan
is to gradually remove some of the neat stuff from Eterm that could be made
generic, such as the theme parsing engine, the command-line options parser or
the event engine, and place it here in the hopes that others will find them
useful.")
    (license license:bsd-2)))
