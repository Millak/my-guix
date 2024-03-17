;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip infinitime)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system node)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization))

(define-public infinitime
  (package
    (name "infinitime")
    (version "1.14.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/InfiniTimeOrg/InfiniTime")
               (commit version)
               (recursive? #t)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1z8mc08jvhpj28w2mk8a2lb3xkxdsxr20f129fjia1i71a2sasim"))
        (snippet
         #~(begin
            (use-modules (guix build utils))
            (substitute* '("src/displayapp/fonts/generate.py"
                           "src/resources/generate-fonts.py")
              (("/usr/bin/env', '") ""))
            ))))
    (build-system cmake-build-system)
    (arguments
     (list
       ;#:target "arm-none-eabi"    ; Not this
       #:target #f
       #:build-type "Release"
       #:configure-flags
       #~(list (string-append "-DARM_NONE_EABI_TOOLCHAIN_PATH="
                              (assoc-ref %build-inputs "gcc-cross-arm-none-eabi-toolchain"))
                              ;(assoc-ref %build-inputs "gcc-cross-sans-libc-arm-none-eabi"))
               (string-append "-DNRF5_SDK_PATH="
                              (assoc-ref %build-inputs "nrf5-sdk"))
               ;; Depends on lv_font_conv node program.
               "-DBUILD_RESOURCES:BOOL=ON"
               ;; Depends on adafruit-nrfutil
               "-DBUILD_DFU:BOOL=ON"
               )
       ;#:make-flags
       ;#~(list "pinetime-app")
       #:modules
       `((srfi srfi-1)
          (guix build utils)
          (guix build cmake-build-system))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-cross-compile-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((libc (assoc-ref inputs "libc"))
                     (gcc (assoc-ref inputs  "gcc")))
                 (setenv "CROSS_C_INCLUDE_PATH"
                         (string-join (fold delete
                                            (string-split (getenv "C_INCLUDE_PATH")
                                                          #\:)
                                            (list ;(string-append libc "/include") ; or add musl
                                                  (string-append gcc "/include")
                                                  (string-append gcc "/include/c++")
                                                  ))
                                      ":"))
                 (setenv "CROSS_CPLUS_INCLUDE_PATH"
                         (string-join ;(cons* (string-append gcc "/include/c++/x86_64-unknown-linux-gnu")
                                             (fold delete
                                            (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                          #\:)
                                            (list (string-append libc "/include")
                                                  (string-append gcc "/include")
                                                  ;(string-append gcc "/include/c++") ; cstdint: No such file or directory
                                                  ))
                                             ;)
                                      ":"))
                 (format #t
                         "environment variable `CROSS_C_INCLUDE_PATH' set to ~a~%"
                         (getenv "CROSS_C_INCLUDE_PATH"))
                 (format #t
                         "environment variable `CROSS_CPLUS_INCLUDE_PATH' set to ~a~%"
                         (getenv "CROSS_CPLUS_INCLUDE_PATH")))))
           #;(replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
                      ))
           )))
    (native-inputs
     (list
           (cross-gcc-toolchain "arm-none-eabi")
           ;(cross-gcc "arm-none-eabi")
           ;(@ (gnu packages musl) musl)
           lv-font-conv
           python
           python-pillow
           ;; for mcuboot http://github.com/JuulLabs-OSS/mcuboot
           ;python-cbor
           ;python-intelhex
           ;python-click
           ;python-cryptography
           ))
    (inputs
     (list adafruit-nrfutil
           nrf5-sdk))
    (home-page "https://infinitime.io/")
    (synopsis "Firmware for the PineTime smartwatch")
    (description "")
    (license license:gpl3+)))

(define-public nrf5-sdk
  (package
    (name "nrf5-sdk")
    (version "15.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://developer.nordicsemi.com/nRF5_SDK"
                            "/nRF5_SDK_v" (version-major version) ".x.x"
                            "/nRF5_SDK_" version "_59ac345.zip"))
        (sha256
         (base32
          "198191665qgpygdyhxx4zbcry6hbq5x58knc5xzh02fiazbhg4ww"))
        (snippet
         #~(begin
            (use-modules (guix build utils))
            (for-each delete-file (find-files "." "\\.(a|hex|lib|msi)$"))
            (delete-file-recursively "external/fatfs/doc")))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("." "." #:exclude-regexp ("examples/")))))
    (native-inputs (list unzip))
    (home-page "https://developer.nordicsemi.com/nRF5_SDK/doc/")
    (synopsis "Development environment for nRF5 Series devices")
    (description "The nRF5 SDK provides a rich developing environment for nRF5
Series devices by including a broad selection of drivers, libraries, examples
for peripherals and SoftDevices.")
    ;; This should be double checked.
    (license license:bsd-3)))

(define-public adafruit-nrfutil
  (package
    (name "adafruit-nrfutil")
    (version "0.5.3.post16")
    (source
      (origin
        ;; Not all files included in the release tarball.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/adafruit/Adafruit_nRF52_nrfutil")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0657fv35khqk5yvizbihh7pz2wpvskx63lb7nnqbq7g56cz3kxsw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             ;; Taken from .github/workflows/githubci.yml
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "adafruit-nrfutil" "version")
                 (invoke "adafruit-nrfutil" "dfu" "genpkg"
                         "--dev-type" "0x0052"
                         "--sd-req" "0x00B6"
                         "--application" "tests/resources/blinky.bin"
                         "blinky.zip")))))))
    (propagated-inputs
     (list python-click python-ecdsa python-pyserial))
    (home-page "https://github.com/adafruit/Adafruit_nRF52_nrfutil")
    (synopsis
     "Adafruit's Nordic Semiconductor nrfutil utility and Python library")
    (description
     "Adafruit's Nordic Semiconductor nrfutil utility and Python library.")
    (license license:bsd-3)))

(define-public lv-font-conv
  (package
    (name "lv-font-conv")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://registry.npmjs.org/lv_font_conv"
                            "/-/lv_font_conv-" version ".tgz"))
        (sha256
         (base32
          "17bhih365p7b4r7vsr5cmfc5vb75v1d2p8sd2jq6kidfahzby87k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (delete-dependencies
                        '(;; dependencies
                          "argparse" "bit-buffer" "make-error" "mkdirp"
                          "opentype.js" "pngjs" "string.prototype.codepointat"
                          "tiny-inflate"
                          ;; dev dependencies
                          "eslint" "file-saver" "mocha" "nyc" "parcel-bundler"
                          "posthtml-include" "roboto-fontface" "shx"))))
                  ;; Use the included dependencies since not all are packaged.
                  (add-after 'unpack 'save-modules
                    (lambda _
                      (copy-recursively "node_modules" "node-modules")))
                  (add-before 'repack 'move-deps
                    (lambda _
                      (copy-recursively "node-modules" "node_modules")
                      (delete-file-recursively "node-modules"))))))
    (inputs
     (list node-debug node-ms))
    (home-page "https://github.com/lvgl/lv_font_conv")
    (synopsis "Font converter to compact bitmap format")
    (description "Converts TTF/WOFF/OTF fonts to compact format, suitable for
small embedded systems.")
    (license license:expat)))
