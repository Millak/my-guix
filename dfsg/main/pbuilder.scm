;;; Copyright © 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main pbuilder)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (srfi srfi-26))

(define-public pbuilder
  (package
    (name "pbuilder")
    (version "0.231")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://salsa.debian.org/pbuilder-team/pbuilder.git/")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0z6f1fgcrkfql9ayc3d0nxra2y6cn91xd5lvr0hd8gdlp9xdvxbc"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:modules `((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-26))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (add-after 'unpack 'patch-source
             (lambda* (#:key inputs outputs #:allow-other-keys)

               ;; Documentation requires tldp-one-page.xsl
               (substitute* "Makefile"
                 ((".*-C Documentation.*") ""))

               ;; Don't create #$output/var/cache/pbuilder/...
               (substitute* '("Makefile"
                              "pbuildd/Makefile")
                 ((".*/var/cache/pbuilder.*") ""))

               ;; Find the correct fallback location.
               (substitute* '("pbuilder-checkparams"
                              "pbuilder-loadconfig"
                              "pbuilder-satisfydepends-apt"
                              "pbuilder-satisfydepends-aptitude"
                              "pbuilder-satisfydepends-classic"
                              "t/test_pbuilder-satisfydepends-classic")
                 (("\\$PBUILDER_ROOT(/usr)?") #$output))

               ;; Some hardcoded paths
               (substitute* '("debuild-pbuilder"
                              "pbuilder"
                              "pbuilder-buildpackage"
                              "pbuilderrc"
                              "pdebuild"
                              "pdebuild-checkparams"
                              "pdebuild-internal")
                 (("/usr/lib/pbuilder")
                  (string-append #$output "/lib/pbuilder")))
               (substitute* "pbuildd/buildd-config.sh"
                 (("/usr/share/doc/pbuilder")
                  (string-append #$output "/share/doc/pbuilder")))
               (substitute* "pbuilder-unshare-wrapper"
                 (("/(s)?bin/ifconfig") "ifconfig")
                 (("/(s)?bin/ip") (search-input-file inputs "/sbin/ip")))
               (substitute* "Documentation/Makefile"
                 (("/usr") ""))

               ;; Ensure PATH works both in Guix and within the Debian chroot.
               (substitute* "pbuilderrc"
                 (("PATH=\"/usr/sbin:/usr/bin:/sbin:/bin")
                  "PATH=\"$PATH:/usr/sbin:/usr/bin:/sbin:/bin"))))
           (add-after 'install 'create-etc-pbuilderrc
             (lambda* (#:key outputs #:allow-other-keys)
               (with-output-to-file (string-append #$output "/etc/pbuilderrc")
                 (lambda ()
                   (format #t "# A couple of presets to make this work more smoothly.~@
                           MIRRORSITE=\"http://deb.debian.org/debian\"~@
                           if [ -r /run/setuid-programs/sudo ]; then~@
                               PBUILDERROOTCMD=\"/run/setuid-programs/sudo -E\"~@
                           fi~@
                           PBUILDERSATISFYDEPENDSCMD=\"~a/lib/pbuilder/pbuilder-satisfydepends-apt\"~%"
                           #$output)))))
           (add-after 'install 'install-manpages
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((man (string-append #$output "/share/man/")))
                 (install-file "debuild-pbuilder.1" (string-append man "man1"))
                 (install-file "pdebuild.1" (string-append man "man1"))
                 (install-file "pbuilder.8" (string-append man "man8"))
                 (install-file "pbuilderrc.5" (string-append man "man5")))))
           (add-after 'install 'wrap-programs
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (for-each
                 (lambda (file)
                   (wrap-script file
                    `("PATH" ":" prefix
                      ,(map (compose dirname (cut search-input-file inputs <>))
                            (list "/bin/cut"
                                  "/bin/dpkg"
                                  "/bin/grep"
                                  "/bin/perl"
                                  "/bin/sed"
                                  "/bin/which"
                                  "/sbin/debootstrap")))))
                 (cons*
                   (string-append #$output "/bin/pdebuild")
                   (string-append #$output "/sbin/pbuilder")
                   (find-files (string-append #$output "/lib/pbuilder"))))))
           ;; Move the 'check phase to after 'install.
           (delete 'check)
           (add-after 'validate-runpath 'check
             (assoc-ref %standard-phases 'check)))
         #:make-flags
         ;; No PREFIX, use DESTDIR instead.
         #~(list (string-append "DESTDIR=" #$output)
                 (string-append "SYSCONFDIR=" #$output "/etc")
                 (string-append "BINDIR=" #$output "/bin")
                 (string-append "PKGLIBDIR=" #$output "/lib/pbuilder")
                 (string-append "SBINDIR=" #$output "/sbin")
                 (string-append "PKGDATADIR=" #$output "/share/pbuilder")
                 (string-append "EXAMPLEDIR=" #$output "/share/doc/pbuilder/examples")
                 "PBUILDDDIR=/share/doc/pbuilder/examples/pbuildd/")))
    (inputs
     (list dpkg
           debootstrap
           grep
           guile-3.0            ; for wrap-script
           iproute
           perl
           which))
    (native-inputs
     (list man-db
           util-linux))
    (home-page "https://pbuilder-team.pages.debian.net/pbuilder/")
    (synopsis "Personal package builder for Debian packages")
    (description
     "@code{pbuilder} is a personal package builder for Debian packages.
@itemize
@item@code{pbuilder} constructs a chroot system, and builds a package inside the
chroot.  It is an ideal system to use to check that a package has correct
build-dependencies.  It uses @code{apt} extensively, and a local mirror, or a
fast connection to a Debian mirror is ideal, but not necessary.
@item@code{pbuilder create} uses debootstrap to create a chroot image.
@item@code{pbuilder update} updates the image to the current state of
testing/unstable/whatever.
@item@code{pbuilder build} takes a @code{*.dsc} file and builds a binary in the
chroot image.
@item@code{pdebuild} is a wrapper for Debian Developers, to allow running
@code{pbuilder} just like @code{debuild}, as a normal user.
@end itemize")
    (license license:gpl2+)))
