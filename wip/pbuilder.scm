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

(define-module (wip pbuilder)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml)
  )

;; TODO: Wrap bins with Debian's devscripts
(define-public pbuilder
  (package
    (name "pbuilder")
    (version "0.230.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://salsa.debian.org/pbuilder-team/pbuilder.git/")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10hhjd1pk3gysafblvyyla08l82j483lddwz64phkvkgm91xq24l"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script
         (add-after 'unpack 'patch-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("Makefile"
                              "pbuildd/Makefile")
                 (("/usr") "")
                 ;; Skip documentation for now
                 ((".*-C Documentation.*") "")
                 )
               (substitute* '(
                              "debuild-pbuilder"
                              "pbuilder"
                              "pbuilder-checkparams"
                              "pbuilder-loadconfig"
                              "pdebuild"
                              )
                 (("/usr/lib/pbuilder") (string-append out "/lib/pbuilder"))
                 )
               (substitute* "pbuildd/buildd-config.sh"
                 (("/usr/share/doc/pbuilder") (string-append out "/share/doc/pbuilder"))
                 )
               #t)))
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                 (lambda (file)
                   (wrap-program file
                                 `("PATH" ":" prefix (,(dirname (which "sed"))
                                                       ,(dirname (which "readlink"))
                                                       ,(dirname (which "dpkg-architecture"))
                                                       ))))
                 (cons* (string-append out "/bin/pdebuild")
                       (string-append out "/sbin/pbuilder")
                       (find-files (string-append out "/lib/pbuilder") ".")))
               #t)))
         )
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:tests? #f
       ))
    (inputs
     `(
       ))
    (native-inputs
     `(
       ("dpkg" ,dpkg)
       ("grep" ,grep)
       ("libxslt" ,libxslt)
       ("man-db" ,man-db)
       ("perl" ,perl)
       ("po4a" ,po4a)
       ("util-linux" ,util-linux)
       ))
    (home-page "https://pbuilder-team.pages.debian.net/pbuilder/")
    (synopsis "Personal package builder for Debian packages")
    (description
     "@code{pbuilder} is a personal package builder for Debian packages.")
    (license license:gpl2+)))
