;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg contrib zfs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system linux-module)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  )

;; Nix's implementation for comparison:
;; https://github.com/NixOS/nixpkgs/blob/master/pkgs/os-specific/linux/zfs/default.nix
(define-public zfs
  (package
    (name "zfs")
    (version "0.8.2")
    (outputs '("out" "module" "src"))
    (source
      (origin
        (method url-fetch)
          (uri (string-append "https://github.com/zfsonlinux/zfs/releases"
                              "/download/zfs-" version
                              "/zfs-" version ".tar.gz"))
          (sha256
           (base32
            "1f7aig15q3z832pr2n48j3clafic2yk1vvqlh28vpklfghjqwq27"))))
    (build-system linux-module-build-system)
    (arguments
     `(
       ;; The ZFS kernel module should not be downloaded since the license
       ;; terms don't allow for distributing it, only building it locally.
       ;#:substitutable? #f
       ;; Tests cannot run in an unprivileged build environment.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'really-configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "configure"
                 (("-/bin/sh") (string-append "-" (which "sh")))
                 ((" /bin/sh") (string-append " " (which "sh"))))
               (invoke "./configure"
                       "--with-config=all"
                       (string-append "--prefix=" out)
                       (string-append "--with-dracutdir=" out "/lib/dracut")
                       (string-append "--with-udevdir=" out "/lib/udev")
                       (string-append "--with-mounthelperdir=" out "/sbin")
                       (string-append "--with-linux="
                                      (assoc-ref inputs "linux-module-builder")
                                      "/lib/modules/build")))))
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (src (assoc-ref outputs "src"))
                   (util-linux (assoc-ref inputs "util-linux"))
                   (nfs-utils (assoc-ref inputs "nfs-utils")))
               (substitute* "module/zfs/zfs_ctldir.c"
                 (("/usr/bin/env\", \"umount")
                  (string-append util-linux "/bin/umount\", \"-n"))
                 (("/usr/bin/env\", \"mount")
                  (string-append util-linux "/bin/mount\", \"-n")))
               (substitute* "lib/libzfs/libzfs_mount.c"
                 (("/bin/mount") (string-append util-linux "/bin/mount"))
                 (("/bin/umount") (string-append util-linux "/bin/umount")))
               (substitute* "lib/libshare/nfs.c"
                 (("/usr/sbin/exportfs")
                  (string-append nfs-utils "/sbin/exportfs")))
               (substitute* "config/zfs-build.m4"
                 (("\\$sysconfdir/init.d") (string-append out "/etc/init.d")))
               (substitute* '("etc/zfs/Makefile.am"
                              "cmd/zed/Makefile.am")
                 (("\\$\\(sysconfdir)") (string-append out "/etc")))
               (substitute* "cmd/vdev_id/vdev_id"
                 (("PATH=/bin:/sbin:/usr/bin:/usr/sbin")
                  (string-append "PATH="
                                 (dirname (which "chmod")) ":"
                                 (dirname (which "grep")) ":"
                                 (dirname (which "sed")) ":"
                                 (dirname (which "gawk")))))
               (substitute* "contrib/pyzfs/Makefile.in"
                 ((".*install-lib.*") ""))
               (substitute* '("Makefile.am" "Makefile.in")
                 (("\\$\\(prefix)/src") (string-append src "/src"))))
             #t))
         (replace 'build
           (lambda _ (invoke "make")))
         (replace 'install
           (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (moddir (assoc-ref outputs "module"))
                    (kmod   (assoc-ref (or native-inputs inputs) "kmod")))
               (invoke "make" "install"
                       (string-append "DEFAULT_INITCONF_DIR=" out "/etc/default")
                       (string-append "DEPMOD=" kmod "/bin/depmod")
                       (string-append "INSTALL_PATH=" out)
                       (string-append "INSTALL_MOD_PATH=" moddir)
                       "INSTALL_MOD_STRIP=1")
               (install-file "contrib/bash_completion.d/zfs"
                             (string-append out "/share/bash-completion/completions"))
               (symlink "../share/pkgconfig/" (string-append out "/lib/pkgconfig"))
               #t))))))
    (native-inputs
     `(("attr" ,attr)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("eudev" ,eudev)
       ("libaio" ,libaio)
       ("libtirpc" ,libtirpc)
       ("nfs-utils" ,nfs-utils)
       ("openssl" ,openssl)
       ("python" ,python)
       ("python-cffi" ,python-cffi)
       ("util-linux" ,util-linux)
       ("zlib" ,zlib)))
    (home-page "https://zfsonlinux.org/")
    (synopsis "Native ZFS on Linux")
    (description
     "ZFS on Linux is an advanced file system and volume manager which was
originally developed for Solaris and is now maintained by the OpenZFS
community.")
    (license license:cddl1.0)))
