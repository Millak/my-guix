;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (efraim packages firmware)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base))

(define-public firmware-nonfree
  (package
    (name "firmware-nonfree")
    (version "20160110")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/non-free/f/"
                                  name "/" name "_" version ".orig.tar.xz"))
              (sha256
               (base32
                "0g2p1v38l2xk7iiy8s26ax43mzb32l53b1n3bzn5hc0ckb8v6655"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin (use-modules (guix build utils))
                   (let ((PATH (string-append
                                 (assoc-ref %build-inputs tar) "/bin"))
                         (dest (string-append
                                 (assoc-ref %outputs "out") "/lib/firmware")))
                     (setenv "PATH" PATH)
                     (system* "tar" "xf" (assoc-ref %build-inputs "source"))
                     (mkdir-p dest)
                     (delete-file "WHENCE")
                     (delete-file "README")
                     (delete-file "GPL-3")
                     (for-each (lambda (license)
                                 (delete-file license))
                               (find-files "." "^LICEN"))
                     (copy-recursively "." dest)))))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)))
    (home-page (list "https://packages.debian.org/source/sid/firmware-nonfree"
                     "https://git.kernel.org/cgit/linux/kernel/git/firmware/linux-firmware.git"))
    (synopsis "Binary firmware for various drivers in the Linux kernel")
    (description "Binary firmware that's not included in the linux-libre
kernel, and which resides as a separate package in Debian and in a separate git
repo from the mainline Linux kernel.")
    (license license:non-copyleft "")))
