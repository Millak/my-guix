;;; Copyright © 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main dpkg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config))

(define-public dpkg
  (package
    (name "dpkg")
    (version "1.20.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.dpkg.org/git/dpkg/dpkg")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "190q968g1vdz07byvfvc2gladhpq3yl765mfiacglyix3nkisy0j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-version
           (lambda _
             (patch-shebang "get-version")
             (with-output-to-file ".dist-version"
               (lambda () (display ,version)))
             #t))
         (add-after 'unpack 'set-perl-libdir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (perl (assoc-ref inputs "perl")))
               (setenv "PERL_LIBDIR"
                       (string-append out
                                      "/lib/perl5/site_perl/"
                                      ,(package-version perl)))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("perl-io-string" ,perl-io-string)))
    (inputs
     `(("bzip2" ,bzip2)
       ("libmd" ,libmd)
       ("ncurses" ,ncurses)
       ("perl" ,perl)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (home-page "https://wiki.debian.org/Teams/Dpkg")
    (synopsis "Debian package management system")
    (description "This package provides the low-level infrastructure for
handling the installation and removal of Debian software packages.")
    (license license:gpl2+)))
