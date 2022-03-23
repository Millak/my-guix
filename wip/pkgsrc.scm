;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip pkgsrc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bash)
  )

(define-public pkgsrc
  (package
    (name "pkgsrc")
    (version "2021Q4")
    (outputs '("out" "pkg"))
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://cdn.netbsd.org/pub/pkgsrc"
                            "/pkgsrc-" version
                            "/pkgsrc-" version ".tar.xz"))
        (sha256
         (base32 "01d2spynmhnd38knnjbzfbcqnijwmkyayss5kqrih7wh7h2bfsz4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script
         (replace 'build
           (lambda* (#:key inputs outputs parallel-build? #:allow-other-keys)
             (setenv "_PKGSRCDIR" (getcwd))
             (with-directory-excursion "bootstrap"
               (setenv "SH" (search-input-file inputs "/bin/sh"))
               (setenv "CC" ,(cc-for-target))
               (setenv "CONFIG_SHELL" (search-input-file inputs "/bin/sh"))
               (setenv "ID" (search-input-file inputs "/bin/id"))

               (mkdir-p (string-append (assoc-ref outputs "pkg") "/bin"))
               (substitute* "bootstrap"
                 (("-o \\$user -g \\$group ") ""))
               ;(substitute* "shells/mksh/files/main.c"
               ;  (("/usr/sbin") ""))
               (invoke "sh" "bootstrap"
                       ;"--full"        ; no mksh
                       "--unprivileged"
                       "--prefer-pkgsrc" "yes"
                       "--prefix" (assoc-ref outputs "pkg")
                       (if parallel-build?
                         (string-append "--make-jobs=" (number->string (parallel-job-count)))
                         `())
                       ;; Try without cwrappers
                       ;"--cwrappers=no"
                       ))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "." (assoc-ref outputs "out"))))
         )))
    (inputs
     (list bash-minimal))
    (home-page "https://pkgsrc.org/")
    (synopsis "")
    (description "Pkgsrc is a framework for managing third-party software on
UNIX-like systems.  It is very versatile and configurable, supporting building
packages for an arbitrary installation prefix, allowing multiple branches to
coexist on one machine, a build options framework, and a compiler transformation
framework, among other advanced features. Unprivileged use and installation is
also supported.")
    ;; TODO: Figure out the license.
    (license #f)))
