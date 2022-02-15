;;; Copyright © 2018, 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cpuid2cpuflags
  (package
    (name "cpuid2cpuflags")
    (version "11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/mgorny/cpuid2cpuflags/releases/"
                            "download/v" version "/"
                            "cpuid2cpuflags-" version ".tar.bz2"))
        (sha256
         (base32
          "0v63sdch996bfmy6l6hkmqdas2akz3f5w82my4jn5y4h3ss22jsw"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/mgorny/cpuid2cpuflags")
    (synopsis "Tool to generate CPU_FLAGS_* for your CPU")
    (description "Tool to generate CPU_FLAGS_* for your CPU")
    (supported-systems '("x86_64-linux" "i686-linux"
                         "armhf-linux" "aarch64-linux"
                         "powerpc64le-linux"))
    (license license:bsd-2)))
