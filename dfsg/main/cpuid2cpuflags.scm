;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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
    (version "5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/mgorny/" name "/releases/"
                            "download/v" version "/"
                            name "-" version ".tar.bz2"))
        (sha256
         (base32
          "0mlpyc93jrbgy42cv53dp4sn2n8xiai9v28chgqljma0075nfgxr"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/mgorny/cpuid2cpuflags")
    (synopsis "Tool to generate CPU_FLAGS_* for your CPU")
    (description "Tool to generate CPU_FLAGS_* for your CPU")
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux"))
    (license license:bsd-2)))
