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

(define-module (wip pbuilder)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml))

;; TODO: Wrap bins with Debian's devscripts
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
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (add-after 'unpack 'patch-source
             (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
               (let ((out #$output)
                     )
                 (substitute* "Makefile"
                   ;(("/usr") "")
                   ;; Skip documentation for now
                   ((".*-C Documentation.*") ""))
                 (substitute* "Documentation/Makefile"
                   (("/usr") ""))
                 ;; Don't create /var/cache/pbuilder/...
                 (substitute* '("Makefile"
                                "pbuildd/Makefile")
                   ((".*/var/cache/pbuilder.*") ""))
                 (substitute* '(;"pbuilder"     ; nothing to substitute
                                "pbuilder-checkparams"
                                "pbuilder-loadconfig"
                                "pbuilder-satisfydepends-apt"
                                "pbuilder-satisfydepends-aptitude"
                                "pbuilder-satisfydepends-classic"
                                "t/test_pbuilder-satisfydepends-classic")
                              ;(find-files "!Documentation")
                   ;(("\\$\\{PBUILDER_PKGLIBDIR:-\\$PBUILDER_ROOT/usr/lib/pbuilder\\}")
                   ; (string-append "${PBUILDER_PKGLIBDIR:-" out "/lib/pbuilder}"))
                   ;(("\\$\\{PBUILDER_PKGDATADIR:-\\$PBUILDER_ROOT/usr/share/pbuilder\\}")
                   ; (string-append "${PBUILDER_PKGDATADIR:-" out "/share/pbuilder}"))
                   ;(("\\$\\{PBUILDER_SYSCONFDIR:-\\$PBUILDER_ROOT/etc\\}")
                   ; (string-append "${PBUILDER_SYSCONFDIR:-" out "/etc}"))
                   (("\\$PBUILDER_ROOT(/usr)?") out)
                   )
                 ;; Some hardcoded paths
                 (substitute* '("debuild-pbuilder"
                                "pbuilder"
                                "pbuilder-buildpackage"
                                ;"pbuilder-checkparams"
                                ;"pbuilder-loadconfig"
                                "pbuilderrc"
                                "pdebuild"
                                "pdebuild-checkparams"
                                "pdebuild-internal"
                                ;"t/testlib.sh"
                                )
                   (("/usr/lib/pbuilder")
                    (string-append out "/lib/pbuilder")))
                 (substitute* "pbuildd/buildd-config.sh"
                   (("/usr/share/doc/pbuilder")
                    (string-append out "/share/doc/pbuilder")))

                 (define (substitute-file file)
                   (search-input-file
                     (or native-inputs inputs) (string-append "/bin/" file)))

                 (substitute* '(
                                ;"pbuilder-modules"
                                ;"pbuilder-apt-config"
                                ;"pbuilder-buildpackage-funcs"
                                "pbuilder-satisfydepends-checkparams"
                                "pbuilderrc"
                                )
                 ;  (("dpkg --print-architecture")
                 ;   (string-append (substitute-file "dpkg")
                 ;                  " --print-architecture"))
                   ;; Same addition as debootstrap.
                   ;; This is only for pbuilderrc.
                   (("PATH=\"/usr/sbin:/usr/bin:/sbin:/bin")
                    "PATH=\"$PATH:/usr/sbin:/usr/bin:/sbin:/bin")
                   )
                 ;(substitute* "pbuilder-modules"
                 ;  (("dpkg-architecture")
                 ;   (substitute-file "dpkg-architecture"))
                 ;  (("dpkg --compare-versions")
                 ;   (string-append (substitute-file "dpkg")
                 ;                  " --compare-versions"))
                 ;  ((" cut") (string-append " " (substitute-file "cut")))
                 ;  (("\\(date") (string-append "(" (substitute-file "date")))
                 ;  (("grep") (substitute-file "grep"))
                 ;  )
                 ;(substitute* "pbuilder-checkparams"
                 ;  (("readlink") (substitute-file "readlink"))
                 ;  (("sort") (substitute-file "sort"))
                 ;  (("touch") (substitute-file "touch"))
                 ;  )
                 ;(substitute* "pbuilder-createbuildenv"
                 ;  (("mkdir") (substitute-file "mkdir"))
                 ;  (("tail") (substitute-file "tail"))
                 ;  (("which") (substitute-file "which"))
                 ;  )
                 )))
           (add-after 'install 'create-etc-pbuilderrc
             (lambda* (#:key outputs #:allow-other-keys)
               (with-output-to-file (string-append #$output "/etc/pbuilderrc")
                 (lambda ()
                   (format #t "# A couple of presets to make this work more smoothly.~@
                           MIRRORSITE=\"http://deb.debian.org/debian\"~@
                           PBUILDERSATISFYDEPENDSCMD=\"~a/lib/pbuilder/pbuilder-satisfydepends-apt\"~%"
                           #$output)))))
           (add-after 'install-more 'wrap-programs
             (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
               (let ((out #$output))
                 (for-each
                   (lambda (file)
                     (wrap-script file
                      `("PATH" ":" prefix
                        (
                         ;,(string-append (assoc-ref (or native-inputs inputs) "coreutils") "/bin")
                         ;,(string-append (assoc-ref (or native-inputs inputs) "dpkg") "/bin")
                         ;,(string-append (assoc-ref (or native-inputs inputs) "debootstrap") "/sbin")
                         ;,(string-append (assoc-ref (or native-inputs inputs) "grep") "/bin")
                         ;,(string-append (assoc-ref (or native-inputs inputs) "sed") "/bin")
                         ;,(string-append (assoc-ref (or native-inputs inputs) "which") "/bin")
                         ,(dirname (search-input-file (or native-inputs inputs) "/bin/cut"))
                         ,(dirname (search-input-file (or native-inputs inputs) "/bin/dpkg"))
                         ,(dirname (search-input-file (or native-inputs inputs) "/bin/grep"))
                         ,(dirname (search-input-file (or native-inputs inputs) "/bin/sed"))
                         ,(dirname (search-input-file (or native-inputs inputs) "/bin/which"))
                         ,(dirname (search-input-file (or native-inputs inputs) "/sbin/debootstrap"))
                         ))
                      `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB")))))
                   (cons*
                     (string-append out "/bin/pdebuild")
                     (string-append out "/sbin/pbuilder")
                     ;; Probably don't need all of them. Maybe.
                     (find-files (string-append out "/lib/pbuilder")))))))
           ;; Move the 'check phase to after 'install.
           (delete 'check)
           (add-after 'wrap-programs 'check
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
           perl
           which))
    (native-inputs
     (list libxslt
           man-db
           po4a
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
@end itemize
NOTE: For Guix System it is recommended to put
@code{PBUILDERROOTCMD=\"/run/setuid-programs/sudo -E\"} inside of your
@file{~/.pbuilderrc}.")
    (license license:gpl2+)))
