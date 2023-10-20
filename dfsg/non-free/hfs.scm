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

(define-module (dfsg non-free hfs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls))

(define-public hfs
  (let ((commit "a9496556b0a5fa805139ea20b44081d48aae912a")
        (revision "1"))
    (package
      (name "hfs")
      (version (git-version "627.40.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/glaubitz/hfs")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0ahjzq689a2g81f3pbaf5nmfk5ickkysr8nxa0mp3lylc4yxg9wb"))))
      (build-system gnu-build-system)
      (arguments
       (list
         #:tests? #f        ; No tests
         #:make-flags
         #~(list (string-append "CC=" #$(cc-for-target)))
         #:phases
         #~(modify-phases %standard-phases
             (delete 'configure)        ; No configure phase
             (add-before 'build 'pre-build
               (lambda _
                 (substitute* "Makefile"
                   (("-I.*include") "-I../include -I../../include"))))
             (replace 'install
               (lambda _
                 (let ((sbin (string-append #$output "/sbin"))
                       (man8 (string-append #$output "/share/man/man8")))
                   (install-file "newfs_hfs/newfs_hfs" sbin)
                   (install-file "newfs_hfs/newfs_hfs.8" man8)
                   (install-file "fsck_hfs/fsck_hfs" sbin)
                   (install-file "fsck_hfs/fsck_hfs.8" man8))))
             (add-after 'install 'create-symlinks
               (lambda* (#:key outputs #:allow-other-keys)
                 (with-directory-excursion (string-append #$output "/sbin")
                   (symlink "fsck_hfs" "fsck.hfsplus")
                   (symlink "newfs_hfs" "mkfs.hfsplus"))
                 (with-directory-excursion (string-append #$output "/share/man/man8")
                   (symlink "fsck_hfs.8" "fsck.hfsplus.8")
                   (symlink "newfs_hfs.8" "mkfs.hfsplus.8")))))))
      (inputs
       (list libbsd
             openssl
             (list util-linux "lib")))
      (home-page "https://opensource.apple.com/source/diskdev_cmds/")
      (synopsis "@code{mkfs} and @code{fsck} for HFS and HFS+ file systems")
      (description "The HFS+ file system used by Apple Computer for their Mac OS
is supported by the Linux kernel.  Apple provides mkfs and fsck for HFS+ with
the Unix core of their operating system, Darwin.  This package is a port of
Apple's tools for HFS+ filesystems.")
      (license ((@@ (guix licenses) license)
                "Apple \"Open Source\" License 1.2"
                "file://APPLE_LICENSE"
                "This package is non-free")))))
