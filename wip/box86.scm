;;; Copyright © 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
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
    (version "0.3.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ptitSeb/box86")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1g2acbq1sgyxpg7miwvx10836j80mgbisib3rp1snwvmz7024krw"))
        (snippet
         #~(begin
             (use-modules (guix build utils))
             (substitute* "CMakeLists.txt"
               ((" /etc/") " ${CMAKE_INSTALL_PREFIX}/etc/"))
             (delete-file "src/dynarec/arm_printer.c")
             (delete-file "src/dynarec/last_run.txt")
             (delete-file "src/wrapped/generated/functions_list.txt")
             ;(delete-file-recursively "src/wrapped/generated")
             (delete-file-recursively "x86lib")
             ;(delete-file "tests/bash")
             ;(delete-file "tests/extensions/mmx")
             ;(for-each (lambda (file)
             ;            (let ((generated-file (string-drop-right file 2)))
             ;              (when (file-exists? generated-file)
             ;                (delete-file generated-file))))
             ;          (find-files "tests" "\\.c$"))
             ))
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
       ;; Test10 (cppThreads) fails, all tests in our build environment.
       #:tests? #f
       #:configure-flags
       #~(append (cond (#$(target-aarch64?)
                        (list "-DARM64=1"))
                       (#$(target-x86?)
                        (list "-DLD80BITS=1"
                              "-DNOALIGN=1"))
                       #;(#$(target-arm32?)
                        (list "-DARM_DYNAREC=1"))
                       ((and #$(target-powerpc?)
                             #$(target-little-endian?))
                        (list "-DPOWERPCLE=1"))
                       (#t '()))
                 (list "-DNOGIT=1"
                       ;; Don't install vendored libraries.
                       "-DNO_LIB_INSTALL=1"
                       ))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'rebuild-wrapped-files
             (lambda _
               (invoke "python3" "rebuild_printer.py" (getcwd))
               ;(invoke "python3" "rebuild_wrappers.py" (getcwd))
               ))
           (add-after 'unpack 'adjust-version-number
             (lambda _
               (substitute* "src/build_info.c"
                 (("nogit") ""))))
           #;(replace 'install
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
    (version "0.2.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ptitSeb/box64")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fpll9x3r8dyfacrww00nlnz9kn4pvhvx19ij3x1cnc4wq32g9kq"))
        (snippet
         #~(begin
             (use-modules (guix build utils))
             (substitute* "CMakeLists.txt"
               ((" /etc/") " ${CMAKE_INSTALL_PREFIX}/etc/"))
             ;(delete-file "src/dynarec/arm64_printer.c")
             ;(delete-file "src/dynarec/last_run.txt")
             (delete-file "src/wrapped/generated/functions_list.txt")
             ;(delete-file-recursively "src/wrapped/generated")
             (delete-file-recursively "x64lib")
             ))
        ))
    (build-system cmake-build-system)
    (arguments
     (list
       ;; Nearly all the tests fail in our build environment.
       ;#:tests? #f
       #:configure-flags
       #~(append (cond (#$(target-aarch64?)
                        (list "-DARM_DYNAREC=ON"
                              "-DPAGE16K=ON"
                              ;"-DRPI5ARM64=ON"
                              ))
                       (#$(target-x86-64?)
                        (list "-DLD80BITS=ON"
                              "-DNOALIGN=ON"))
                       (#$(target-riscv64?)
                        (list "-DRV64=ON"
                              "-DRV64_DYNAREC=ON"))
                       (#$(target-ppc64le?)
                        (list "-DPPC64LE=ON"))
                       (#t '()))
                 (list "-DNOGIT=ON"
                       "-DSAVE_MEM=ON"
                       ;; Don't install vendored libraries.
                       "-DNO_LIB_INSTALL=ON"))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-version-number
             (lambda _
               (substitute* "src/build_info.c"
                 (("nogit") ""))))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "./box64" "-v"))))
           (replace 'install
             (lambda _
               (install-file "box64" (string-append #$output "/bin")))))))
    (native-inputs
     (list python-minimal))
    (home-page "https://box86.org/")
    (synopsis "Linux Userspace x86_64 Emulator")
    (description "Box64 lets you run x86_64 Linux programs (such as games) on
non-x86_64 Linux systems, like ARM (host system needs to be 64bit
little-endian).")
    (supported-systems '("x86_64-linux" "aarch64-linux"
                         "riscv64-linux" "powerpc64le-linux"))
    (license license:expat)))
