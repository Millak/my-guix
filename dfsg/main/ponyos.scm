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

(define-module (dfsg main ponyos)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages virtualization))

(define-public ponyos
  (package
    (name "ponyos")
    (version "5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/klange/ponyos/"
                            "releases/download/v" version "/ponyos.iso"))
        (sha256
         (base32
          "1cd6il5ckbphy2mn9wwf7wycc0cj1b4pb4nq6kwsb2np7j9m51k4"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (let* ((out     (assoc-ref %outputs "out"))
                (bin     (string-append out "/bin"))
                (icon    (assoc-ref %build-inputs "icon"))
                (qemu    (assoc-ref %build-inputs "qemu"))
                (source  (assoc-ref %build-inputs "source"))
                (desktop (string-append out "/share/applications")))
           (use-modules (guix build utils))
           (mkdir-p bin)
           (mkdir-p desktop)
           (copy-file icon (string-append out "/share/logo-new.png"))
           (call-with-output-file
             (string-append bin "/ponyos.sh")
             (lambda (file)
               (format file
                       "#!/bin/sh
~a/bin/qemu-system-i386 -m 1G -enable-kvm -soundhw ac97 -cdrom ~a \n"
                       qemu source)))
           (chmod (string-append bin "/ponyos.sh") #o555)

           (call-with-output-file
             (string-append desktop "/ponyos.desktop")
             (lambda (file)
               (format file
                       "[Desktop Entry]~@
                       Name=ponyos~@
                       Comment=An OS for Everypony~@
                       Exec=~a/bin/qemu-system-i386 -m 1G -enable-kvm -soundhw ac97 -cdrom ~a~@
                       TryExec=~@*~a/bin/qemu-system-i386 -m 1G -enable-kvm -soundhw ac97 -cdrom ~a~@
                       Type=Application~%"
                       qemu source)))))))
    (inputs
     `(("qemu" ,qemu)
       ("icon" ,(origin
                  (method url-fetch)
                  (uri "http://ponyos.org/assets/img/logo-new.png")
                  (sha256
                    (base32
                      "04i9lykh87gfgln4bagqrp7bxsy6xp71x0s566bahkyychbmchz7"))))))
    (home-page "http://ponyos.org/")
    (synopsis "An OS for Everypony")
    (description "PonyOS is a hobby Unix-like operating system, designed for
ponies like you!  PonyOS uses its own kernel, built from scratch.  The CD
should boot in QEMU, VirtualBox, VMWare, or even on real hardware if you're
a brave pony!")
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:expat)))
