;;; Copyright © 2018, 2020, 2022, 2025, 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main cpuid2cpuflags)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cpuid2cpuflags
  (package
    (name "cpuid2cpuflags")
    (version "18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gentoo/cpuid2cpuflags")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "045xvld4ybavg544blbckf66lc0km1pamnhj0r07iwdb7lm9r6mp"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake))
    (home-page "https://github.com/gentoo/cpuid2cpuflags")
    (synopsis "Tool to generate CPU_FLAGS_* for your CPU")
    (description
     "This program attempts to obtain the identification and
capabilities of the currently used CPU, and print the matching set of
CPU_FLAGS_* flags for Gentoo.")
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux"
                         "aarch64-linux" "powerpc-linux" "powerpc64le-linux"))
    (license license:gpl2+)))
