;;; Copyright © 2025 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg non-free amscope)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:hide (freetype zlib))
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap) ; glibc-dynamic-linker
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)
  )

(define-public amlite-md500
  (package
    (name "amlite-md500")
    (version "20232603")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://storage.googleapis.com/"
                            "software-download-d79bb.appspot.com/"
                            "software/AmLite/Linux/" version
                            "/AmScopeAmLite.x64.tar.bz2"))
        (sha256
         (base32
          "0xn63pkm8fq29i1y9b9ls6s5izglb9cdn6ijllzx1c391fi3f80v"))))
    (build-system copy-build-system)
    (arguments
     (list
       #:install-plan
       ''(("AmLite" "libexec/amlite/")
          ("libamcam.so" "libexec/amlite/")
          ("libamnam.so" "libexec/amlite/")
          ("libamsam.so" "libexec/amlite/")
          ("libimagepro.so" "libexec/amlite/")
          ("AmLite.png" "libexec/amlite/")
          ("i18n" "libexec/amlite/")
          ("99-amcam.rules" "etc/udev/rules.d/")
          ("AmLite.png" "share/icons/hicolor/128x128/apps/")
          ("AmLite.desktop" "share/applications/"))
       #:phases
       #~(modify-phases %standard-phases
           (replace 'unpack
             (lambda _
               (invoke "tar" "xf" #$source)
               (with-output-to-file "AmLite.tgz"
                 (lambda _
                   (invoke "sed" "-n" "-e" "1,/^exit 0$/!p" "AmScopeAmLite.x64.sh")))
               (invoke "tar" "xvf" "AmLite.tgz")))
           (add-before 'install 'patch-desktop-file
             (lambda _
               (substitute* "AmLite.desktop"
                 (("Exec=/usr/local/AmLite")
                  (string-append "Exec=" #$output "/bin"))
                 (("Icon=/usr/local/AmLite")
                  (string-append "Icon=" #$output "/share/icons/hicolor/128x128/apps")))))
           (add-before 'install 'patchelf
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((ld-so  (search-input-file inputs #$(glibc-dynamic-linker)))
                      (rpath  (string-join
                                (list (dirname ld-so)
                                      (dirname
                                        (search-input-file inputs "/lib/libz.so.1"))
                                      (dirname
                                        (search-input-file inputs "/lib/libfontconfig.so.1"))
                                      (dirname
                                        (search-input-file inputs "/lib/libfreetype.so.6"))
                                      (dirname
                                        (search-input-file inputs "/lib/libxcb-glx.so.0"))
                                      (dirname
                                        (search-input-file inputs "/lib/libX11-xcb.so.1"))
                                      (dirname
                                        (search-input-file inputs "/lib/libX11.so.6"))
                                      (dirname
                                        (search-input-file inputs "/lib/libXrender.so.1"))
                                      (dirname
                                        (search-input-file inputs "/lib/libxcb.so.1"))
                                      (dirname
                                        (search-input-file inputs "/lib/libGL.so.1"))
                                      (dirname
                                        (search-input-file inputs "/lib/libstdc++.so.6"))
                                      (dirname
                                        (search-input-file inputs "/lib/libgcc_s.so.1")))
                                ":")))
                 (invoke "patchelf" "--set-rpath" (dirname ld-so) "libamcam.so")
                 (invoke "patchelf" "--set-rpath" (dirname ld-so) "libamnam.so")
                 (invoke "patchelf" "--set-rpath" (dirname ld-so) "libamsam.so")
                 (invoke "patchelf" "--set-rpath"
                         (string-join
                           (list
                             (dirname (search-input-file inputs "/lib/libz.so.1"))
                             (dirname ld-so))
                           ":")
                         "libimagepro.so")
                 (invoke "patchelf" "--set-rpath" rpath "AmLite")
                 ;; cannot find section '.interp'. The input file is most likely statically linked
                 ;(invoke "patchelf" "--set-interpreter" ld-so "libamcam.so")
                 ;(invoke "patchelf" "--set-interpreter" ld-so "libamnam.so")
                 ;(invoke "patchelf" "--set-interpreter" ld-so "libamsam.so")
                 ;(invoke "patchelf" "--set-interpreter" ld-so "libimagepro.so")
                 (invoke "patchelf" "--set-interpreter" ld-so "AmLite"))))
           (add-after 'install 'wrap-binary
             (lambda _
               (wrap-program (string-append #$output "/libexec/amlite/AmLite")
                 '("QT_QPA_PLATFORM" ":" = ("xcb")))
               (with-directory-excursion #$output
                 (mkdir "bin")
                 (rename-file "libexec/amlite/AmLite" "bin/AmLite")
                 (rename-file "libexec/amlite/.AmLite-real" "libexec/amlite/AmLite")
                 (substitute* "bin/AmLite"
                   (("\\.AmLite-real") "AmLite"))))))
         #:substitutable? #f        ; Non-redistributable.
         #:strip-binaries? #f       ; Causes RUNPATH validation to fail.
         #:tests? #f))              ; No tests.
    (native-inputs (list patchelf sed))
    (inputs (list fontconfig
                  freetype
                  libxrender
                  `(,gcc "lib")
                  mesa
                  zlib))
    (home-page "https://amscope.com/products/md500")
    (synopsis "Camera software for the AmScope MD series 5.0 MP camera")
    (description "Enhance your microscopy experience by downloading the
essential software for your microscope cameras.  This software provides
advanced features such as image capture, measurement tools, and editing
capabilities, allowing you to maximize the potential of your microscope.
Compatible with various camera models, it ensures seamless integration and
optimal performance.")
    (supported-systems '("x86_64-linux"))
    (license ((@@ (guix licenses) license)
              "Nonfree"
              "https://cdn.shopify.com/s/files/1/0526/7285/6239/files/AMSCOPE_SOFTWARE_LICENSE_TERMS.pdf?v=1721669670"
              "This package is non-free"))))
