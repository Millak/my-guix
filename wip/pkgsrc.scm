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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (srfi srfi-1)
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
       #:implicit-inputs? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script
         (delete 'patch-generated-file-shebangs)
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
     (list
       ;bash-minimal
       ))
    (native-inputs
      ((@@ (gnu packages commencement) %bootstrap-inputs+toolchain)
     ;`(("libc" ,(@@ (gnu packages commencement) glibc-mesboot))
     ;  ,@(alist-delete "libc" ((@@ (gnu packages commencement) %boot-mesboot3-inputs)))
     ;`(("bash" ,(@@ (gnu packages commencement) bash-mesboot))
     ;  ("binutils" ,(@@ (gnu packages commencement) binutils-mesboot1))
     ;  ("coreutils" ,(@@ (gnu packages commencement) coreutils-mesboot0))
     ;  ("gawk" ,(@@ (gnu packages commencement) gawk-mesboot))
     ;  ("grep" ,(@@ (gnu packages commencement) grep-mesboot))
     ;  ("make" ,(@@ (gnu packages commencement) gnu-make-mesboot))
     ;  ("sed" ,(@@ (gnu packages commencement) sed-mesboot))
     ;  ("tar" ,(@@ (gnu packages commencement) tar-mesboot))
     ;  ,@(fold alist-delete ((@@ (gnu packages commencement) %boot-mesboot0-inputs))
     ;          '("bash" "binutils" "bootar" "coreutils" "gash"
     ;            "gawk" "grep" "guile" "make" "sed" "tar"))
     ;  ("xz" ,xz)
       )
     ;(%boot-mesboot1-inputs) ; already a list
     ;(list
     ;  ;(list gcc-toolchain "static")
     ;  ;%binutils-static
     ;  (static-package binutils)
     ;  (static-package coreutils)
     ;  (static-package grep)
     ;  (static-package sed)
     ;  ;coreutils-mesboot0
     ;  ;(%boot-mesboot1-inputs) ; already a list
     ;  static-bash
     ;  static-gawk
     ;  tar
     ;  xz
     ;  )
     )
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

(define static-gawk
  (let ((base (static-package gawk)))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:tests? _ #f) #f))))))
