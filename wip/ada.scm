;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip ada)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages texinfo)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public gcc-2.7
  (package
    (inherit gcc)
    (version "2.7.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-" version ".tar.gz"))
              (sha256
               (base32
                "0g3pffmbkm45kfidfc93mrakivv5j1b50rki2hqvnr101w7nw5hn"))
              ;(snippet
              ; #~(begin
              ;     ;; Need a bison older than 3.0.5.
              ;     (delete-file "objc-parse.c")
              ;     (delete-file "cexp.c")
              ;     (delete-file "c-parse.c")
              ;     (delete-file "bi-parser.c")
              ;     (delete-file "cp/parse.c")))
              ))
    (supported-systems (fold delete %supported-systems
                             '("powerpc64le-linux" "riscv64-linux")))
    (outputs '("out"))
    (arguments
     (let ((matching-system
             (match (%current-system)
               ;; This package predates our 64-bit architectures.
               ;; Force a 32-bit build targeting a similar architecture.
               ("aarch64-linux"
                "armhf-linux")
               ("x86_64-linux"
                "i686-linux")
               (_
                (%current-system)))))
       (list #:system matching-system
             #:configure-flags #~'("--disable-werror")
             #:tests? #f        ; There does not seem to be a test suite.
             #:parallel-build? #f
             #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target))
                     ;; Despite objections to the contrary, OLDCC is still gcc.
                     (string-append "OLDCC=" #$(cc-for-target))
                     ;; We need to add the -O1 when building with gcc-11.
                     "CFLAGS = -O1 -g"
                     ;; badly punctuated parameter list in `#define'
                     ;; None of these seem to ignore it :/
                     ;"ENQUIRE_CFLAGS = -Wp,-std=c9x -DNO_MEM -DNO_LONG_DOUBLE_IO -O0"
                     ;"ENQUIRE_CFLAGS = -w -DNO_MEM -DNO_LONG_DOUBLE_IO -O0"
                     ;; Drop objc
                     "LANGUAGES = c proto")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'replace-deprecated-symbols
                   (lambda _
                     ;; sys_nerr and sys_errlist removed in glibc-2.32.
                     (substitute* '("protoize.c"
                                    "gcc.c"
                                    "cpplib.c"
                                    "collect2.c"
                                    "cccp.c"
                                    "cp/g++.c")
                       (("sys_nerr") "strerrorname_np")
                       (("sys_errlist") "strerrordesc_np"))
                     ;; At some point gcc stops implementing varargs.h
                     ;; and it moved to glibc.
                     (substitute* (find-files "." "\\.c$")
                       (("#include <varargs\\.h>")
                        "#include <stdarg.h>"))
                     (substitute* "protoize.c"
                       (("#include <stdio\\.h>")
                        "#include <stdio.h>\n#include <string.h>"))))
                 (add-before 'configure 'set-dynamic-linker-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Tell GCC what the real loader file name is.
                     (substitute* "config/m68k/linux.h"
                       (("/lib/ld\\.so\\.1")
                        (search-input-file
                          inputs #$(glibc-dynamic-linker matching-system))))
                     (substitute* "config/i386/gnu.h"
                       (("/lib/ld\\.so")
                        (search-input-file
                          inputs #$(glibc-dynamic-linker matching-system))))
                     (substitute* '("config/i386/linux.h"
                                    "config/m68k/linux.h")
                       (("/lib(64)?/ld-linux\\.so\\.[12]")
                        (search-input-file
                          inputs #$(glibc-dynamic-linker matching-system))))))
                 (replace 'configure
                   (lambda* (#:key outputs build configure-flags
                             #:allow-other-keys)
                     ;; It's an old 'configure' script so it needs some help.
                     (setenv "CONFIG_SHELL" (which "sh"))
                     (apply invoke "./configure"
                            (string-append "--prefix=" #$output)
                            (string-append "--build=" build)
                            (string-append "--host=" build)
                            configure-flags)))
                 (add-before 'configure 'patch-more-shebangs
                   (lambda _
                     (substitute* "genmultilib"
                       (("#!/bin/sh") (string-append "#!" (which "sh"))))
                     (substitute* "configure"
                       (("`/bin/sh") (string-append "`" (which "sh"))))
                     (substitute* "Makefile.in"
                       (("SHELL = /bin/sh")
                        (string-append "SHELL = " (which "sh"))))))))))
    (native-inputs
     (list texinfo
           ;; See above about badly punctuated parameter list.
           (@ (gnu packages base) glibc-2.33)))
    (inputs '())
    (propagated-inputs '())
    (native-search-paths
     ;; This package supports nothing but the C language.
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))))))

(define-public gnat-3.09
  (package
    (inherit gcc-2.7)
    (name "gnat")
    (version "3.09")
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-2.7)
       ((#:make-flags flags)
        ;; TODO: Inherit from gcc-2.7.
        ;#~(append
        ;    (remove (cut string-match "LANGUAGES" <>)
        ;            #$flags)
        ;    (list "LANGUAGES = c ada")))
        #~(list (string-append "CC=" #$(cc-for-target))
                ;; Despite objections to the contrary, OLDCC is still gcc.
                (string-append "OLDCC=" #$(cc-for-target))
                ;; We need to add the -O1 when building with gcc-11.
                "CFLAGS = -O1 -g"
                ;; badly punctuated parameter list in `#define'
                ;; None of these seem to ignore it :/
                ;"ENQUIRE_CFLAGS = -Wp,-std=c9x -DNO_MEM -DNO_LONG_DOUBLE_IO -O0"
                ;"ENQUIRE_CFLAGS = -w -DNO_MEM -DNO_LONG_DOUBLE_IO -O0"
                ;; Drop objc and proto
                "LANGUAGES = c ada"))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'unpack-ada-source
              (lambda* (#:key inputs #:allow-other-keys)
                (invoke "tar" "xf" (assoc-ref inputs "ada-source"))
                (rename-file "gnat-3.09.orig/ada" "ada")
                ;; Using 'invoke patch' returns the following error:
                ;; Hmm...  I can't seem to find a patch in there anywhere.
                ;; Verbose patching so we can see the output in the logs.
                (system
                  (string-append "patch --verbose -p0 < "
                                 "gnat-3.09.orig/gnat-3.09-src/src/gcc-272.dif"))
                (with-directory-excursion "ada"
                  (system
                    (string-append "patch --verbose -p0 < "
                                   "../gnat-3.09.orig/gnat-3.09-src/src/linux.dif")))))
                 ;; These are the instructions, but is it necessary?
                 (replace 'build
                   (lambda* (#:key (make-flags '()) #:allow-other-keys)
                     (apply invoke "make" make-flags)
                     (apply invoke "make" "bootstrap" make-flags)
                     (apply invoke "make" "gnatlib_and_tools" make-flags)))))))
    (native-inputs
     `(("ada-source"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://snapshot.debian.org/archive"
                               "/debian-archive/20090802T004153Z/debian"
                               "/dists/bo/main/source/devel/gnat_"
                               version ".orig.tar.gz"))
           (sha256
            (base32 "1mw5qyss7lm59mhrxzzjkfa3bnqmq169w5ii8jy6m2868nz3r890"))))
       ;("ada" ,(@ (gnu packages ada) ada/ed))   ; This isn't enough to build gnat.
       ,@(package-native-inputs gcc-2.7)))
    (native-search-paths
     ;; TODO: Add the search-path-specification for ADA.
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))))))
