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

(define-module (dfsg non-free b43-firmware)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (dfsg contrib b43-fwcutter))

(define-public broadcom-wl
  (package
    (name "broadcom-wl")
    (version "6.30.163.46")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.lwfinger.com/b43-firmware/"
                            "broadcom-wl-" version ".tar.bz2"))
        (sha256
         (base32
          "0baw6gcnrhxbb447msv34xg6rmlcj0gm3ahxwvdwfcvq4xmknz50"))))
    (build-system copy-build-system)
    (arguments
     (list
       #:substitutable? #f          ; Non-redistributable.
       #:strip-binaries? #f         ; Pre-compiled firmware
       #:install-plan
       #~'(("b43" "lib/firmware/"))
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'install 'build
             (lambda _
               (let ((wl_apsta.o
                       (string-append #$name "-" #$version ".wl_apsta.o")))
                 (invoke "b43-fwcutter" "-w" "." wl_apsta.o)))))))
    (inputs (list b43-fwcutter))
    (home-page "https://wireless.wiki.kernel.org/en/users/Drivers/b43")
    (synopsis "Broadcom Wireless Driver")
    (description "This package provides the firmware needed by the b43 kernel
driver for some Broadcom 43xx wireless network cards.
Supported chipsets:
@enumerate
@item BCM4306/3 (chip revision 3 only);
@item BCM4311 (NOT PCI Id 14e4:4313);
@item BCM4312;
@item BCM43131;
@item BCM4318;
@item BCM4321 (only partial support, not all versions tested);
@item BCM43217;
@item BCM4322 (only partial support for some versions, not all versions tested);
@item BCM43222 (not all versions tested);
@item BCM43224 (not all versions tested);
@item BCM43225;
@item BCM43227;
@item BCM43228;
@item BCM4331;
@item BCM47xx (detection not reliable, may not support all versions)
@end enumerate")
    (license ((@@ (guix licenses) license)
              "Nonfree"
              "https://wireless.wiki.kernel.org/en/users/Drivers/b43"
              "This package is non-free"))))

(define-public broadcom-wl-5
  (package
    (inherit broadcom-wl)
    (name "broadcom-wl")
    (version "5.100.138")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.lwfinger.com/b43-firmware/"
                            "broadcom-wl-" version ".tar.bz2"))
        (sha256
         (base32
          "0vz4ka8gycf72gmnaq61k8rh8y17j1wm2k3fidxvcqjvmix0drzi"))))
    (arguments
     (substitute-keyword-arguments (package-arguments broadcom-wl)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda _
                (let ((wl_apsta.o "linux/wl_apsta.o"))
                  (invoke "b43-fwcutter" "-w" "." wl_apsta.o))))))))))

(define-public broadcom-wl-legacy
  (package
    (inherit broadcom-wl)
    (name "broadcom-wl")
    (version "3.130.20.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://sources.openwrt.org/"
                            "wl_apsta-" version ".o"))
        (sha256
         (base32
          "147gqfkldp5nssq3a02wdynb3lgcsr6f3g5w07li9pcn3l5n3fkx"))))
    (arguments
     (substitute-keyword-arguments (package-arguments broadcom-wl)
       ((#:install-plan _ #~'())
        #~'(("b43legacy" "lib/firmware/")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'unpack)                ; Single file.
            (replace 'build
              (lambda _
                (invoke "b43-fwcutter" "-w" "." #$source)))))))
    (description "This package provides the firmware needed by the b43legacy
kernel driver for some Broadcom 43xx wireless network cards.
Supported chipsets:
@enumerate
@item BCM4301;
@item BCM4306/2;
@item BCM4306
@end enumerate")))
