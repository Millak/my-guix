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

(define-module (dfsg main mac-fdisk)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public mac-fdisk
  (let ((debian-patch-level "18.1"))
    (package
      (name "mac-fdisk")
      (version "0.1")
      (source
        (origin
          (method url-fetch)
          (uri (string-append "mirror://debian/pool/main/m/mac-fdisk/"
                              "mac-fdisk_" version ".orig.tar.gz"))
          (sha256
           (base32 "0rkaqp82l47pg0ymqys07mljf3widv2yk4hhgs2yz8hwli5zqnbh"))
          (patches
            (list
              (origin (method url-fetch)
                      (uri (string-append "https://sources.debian.org/data/main/m/"
                                          "mac-fdisk/" version "-" debian-patch-level
                                          "/debian/patches/debian.patch"))
                      (file-name (string-append name "-debian-" version "-"
                                                debian-patch-level ".patch"))
                      (sha256
                       (base32
                        "1r38nh4d0l88n5jqvcyqcwpka2krycbzz6df6a21i3ph0awd8nza")))))
          (snippet
           #~(begin (use-modules (guix build utils))
                    (substitute* "errors.c"
                      (("#ifndef __linux__") "#ifndef __foo__")
                      (("#ifdef __linux__") "#ifdef __foo__"))
                    ;; Enable support for x86_64-linux.
                    (substitute* '("fdisklabel.c"
                                   "fdisklabel.h")
                      (("#if defined \\(i386\\)")
                       "#if defined (i386) || defined (__amd64)"))))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags '("CFLAGS=-O2 -g")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)        ; no configure script.
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (mac-fdisk.8 (assoc-ref inputs "mac-fdisk.8"))
                      (pmac-fdisk.8 (assoc-ref inputs "pmac-fdisk.8"))
                      (sbin (string-append out "/sbin"))
                      (man8 (string-append out "/share/man/man8")))
                 (mkdir-p sbin)
                 (mkdir-p man8)
                 (copy-file "fdisk" (string-append sbin "/mac-fdisk"))
                 (copy-file "pdisk" (string-append sbin "/pmac-fdisk"))
                 (copy-file mac-fdisk.8 (string-append man8 "/mac-fdisk.8"))
                 (copy-file pmac-fdisk.8 (string-append man8 "/pmac-fdisk.8"))))))
         #:make-flags (list (string-append "CC=" ,(cc-for-target)))
         #:tests? #f))                ; no tests
      (inputs
       `(("mac-fdisk.8"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://sources.debian.org/data/main/m/mac-fdisk/"
                                 version "-" debian-patch-level
                                 "/debian/mac-fdisk.8.in"))
             (sha256
              (base32 "0nn21ahsqadccmbixpxcpxi835lsh0iq0n04dpr5vac6pni6mn78"))))
         ("pmac-fdisk.8"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://sources.debian.org/data/main/m/mac-fdisk/"
                                 version "-" debian-patch-level
                                 "/debian/pmac-fdisk.8.in"))
             (sha256
              (base32 "1rdmixidgwmgs95n65nvwlqkm9wfbsfj58qdan56vw8mbldns86x"))))))
      (home-page "https://tracker.debian.org/pkg/mac-fdisk")
      (synopsis "Apple disk partition manipulation tool")
      (description "The @code{fdisk} utilities from the MkLinux project, adopted
for Linux/m68k.  @code{mac-fdisk} allows you to create and edit the partition
table of a disk.  It supports only the Apple partition format used on Macintosh
and PowerMac, use @code{pmac-fdisk} for PC partition format disks as used on
PowerPC machines.  @code{mac-fdisk} is an interactive tool with a menu similar
to PC @code{fdisk}, supported options are somewhat different from PC
@code{fdisk} due to the differences in partition format.")
      (supported-systems '("powerpc-linux" "powerpc64le-linux"
                           "i686-linux" "x86_64-linux"))
      (license license:gpl2))))
