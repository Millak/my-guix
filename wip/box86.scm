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

(define-module (wip box86)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages python)
  #:use-module (ice-9 match))

;; TODO: This package has many pre-generated files
(define-public box86
  (package
    (name "box86")
    (version "0.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ptitSeb/box86")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "17kq8iyv935krlza75i6vkszaz2lpb5w6m6nxrjjkwi7dky0mwwx"))
        ;(snippet
        ; #~(begin
        ;     (use-modules (guix build utils))
        ;     (delete-file "src/dynarec/arm_printer.c")
        ;     (delete-file-recursively "src/wrapped/generated")
        ;     (delete-file-recursively "x86lib")))
        ))
    (build-system cmake-build-system)
    (arguments
     (list
       ;; We need the 32-bit variants of the architectures.
       #:system
       (match (%current-system)
              ("aarch64-linux"
               "armhf-linux")
              ("x86_64-linux"
               "i686-linux")
              (_
                (%current-system)))
       #:out-of-source? #f
       ;; Test10 (cppThreads) fails, all tests in our build environment.
       #:tests? #f
       #:configure-flags
       #~(append (cond (#$(target-aarch64?)
                        (list "-DARM64=1"))
                       (#$(target-x86?)
                        (list "-DLD80BITS=1"
                              "-DNOALIGN=1"))
                       (#$(target-arm32?)
                        (list "-DARM_DYNAREC=1"))
                       ((and #$(target-powerpc?)
                             #$(target-little-endian?))
                        (list "-DPOWERPCLE=1"))
                       (#t '()))
                 (list "-DNOGIT=1"))
       #:phases
       #~(modify-phases %standard-phases
           ;(add-after 'unpack 'rebuild-wrapped-files
           ;  (lambda _
           ;    (invoke "python3" "rebuild_printer.py" (getcwd))
           ;    (invoke "python3" "rebuild_wrappers.py" (getcwd))))
           (add-after 'unpack 'adjust-version-number
             (lambda _
               (substitute* "src/build_info.c"
                 (("nogit") ""))))
           ;(add-after 'unpack 'always-build-binfmt-file
           ;  (lambda _
           ;    (substitute* "CMakeLists.txt"
           ;      (("enable_testing\\(\\)")
           ;       (string-append "enable_testing()\n"
           ;                      "configure_file(system/box86.conf.cmake system/box86.conf)")))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "box86"
                               (string-append out "/bin"))
                 (install-file "system/box86.box86rc"
                               (string-append out "/etc"))
                 (unless #$(target-x86?)
                   (install-file "system/box86.conf"
                                 (string-append out "/etc/binfmt.d")))))))))
    (native-inputs
     (list python))
    (home-page "https://box86.org/")
    (synopsis "Linux Userspace x86 Emulator")
    (description "Box86 lets you run x86 Linux programs (such as games) on
non-x86 Linux systems, like ARM (host system needs to be 32bit little-endian).")
    (license license:expat)))

(define-public box64
  (package
    (name "box64")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ptitSeb/box64")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0hdrqngz6crdhwnkd8smrk8nc4s7gcfw92shv9rzklr4gp8cp2v8"))
        ;(snippet
        ; #~(begin
        ;     (use-modules (guix build utils))
        ;     (delete-file "src/dynarec/arm_printer.c")
        ;     (delete-file-recursively "src/wrapped/generated")
        ;     (delete-file-recursively "x64lib")))
        ))
    (build-system cmake-build-system)
    (arguments
     (list
       #:out-of-source? #f
       ;; Nearly all the tests fail in our build environment.
       #:tests? #f
       #:configure-flags
       #~(append (cond (#$(target-aarch64?)
                        (list "-DARM_DYNAREC=ON"))
                       (#$(target-x86-64?)
                        (list "-DLD80BITS=1"
                              "-DNOALIGN=1"))
                       (#$(target-riscv64?)
                        (list "-DRV64=1"))
                       (#$(target-ppc64le?)
                        (list "-DPPC64LE=1"))
                       (#t '()))
                 (list "-DNOGIT=1"))
       #:phases
       #~(modify-phases %standard-phases
           ;(add-after 'unpack 'rebuild-wrapped-files
           ;  (lambda _
           ;    (invoke "python3" "rebuild_printer.py" (getcwd))
           ;    (invoke "python3" "rebuild_wrappers.py" (getcwd))))
           (add-after 'unpack 'adjust-version-number
             (lambda _
               (substitute* "src/build_info.c"
                 (("nogit") ""))))
           ;(add-after 'unpack 'always-build-binfmt-file
           ;  (lambda _
           ;    (substitute* "CMakeLists.txt"
           ;      (("enable_testing\\(\\)")
           ;       (string-append "enable_testing()\n"
           ;                      "configure_file(system/box64.conf.cmake system/box64.conf)")))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "box64"
                               (string-append out "/bin"))
                 (install-file "system/box64.box64rc"
                               (string-append out "/etc"))
                 (unless #$(target-x86?)
                   (install-file "system/box64.conf"
                                 (string-append out "/etc/binfmt.d")))))))))
    (native-inputs
     (list python))
    (home-page "https://box86.org/")
    (synopsis "Linux Userspace x86_64 Emulator")
    (description "Box64 lets you run x86_64 Linux programs (such as games) on
non-x86_64 Linux systems, like ARM (host system needs to be 64bit
little-endian).")
    (supported-systems '("x86_64-linux" "aarch64-linux"
                         "riscv64-linux" "powerpc64le-linux"))
    (license license:expat)))
