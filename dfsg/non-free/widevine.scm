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

(define-module (dfsg non-free widevine)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bootstrap) ; glibc-dynamic-linker
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages python))

;; for firefox and icecat?
;; symlink $output/lib/firefox to ${HOME}/.mozilla/firefox/${profile}/gmp-widevinecdm

;; Where did this come from?
;; symlink 'chromium' folder to ${HOME}/.mozilla/firefox/${profile}/gmp-widevinecdm/$version

;; test with `qutebrowser --temp-basedir https://bitmovin.com/demos/drm`

;; https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=chromium-widevine
(define-public widevine-x86_64
  (package
    (name "widevine")
    (version "4.10.2652.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://dl.google.com/linux/deb/"
                            "pool/main/g/google-chrome-stable/"
                            "google-chrome-stable_113.0.5672.126-1_amd64.deb"))
        (sha256
         (base32 "1w8b41ij6xl8byh1977skxwwq0nivpfn8wb3gv12xvm7sw24jqka"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:system "x86_64-linux"
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)      ; No configure script.
           (replace 'unpack
             (lambda _
               (invoke "ar" "x" #$source "data.tar.xz")
               (invoke "tar" "xvf" "data.tar.xz")))
           (add-before 'build 'change-directory
             (lambda _
               (chdir "opt/google/chrome/WidevineCdm")))
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((widevine "_platform_specific/linux_x64/libwidevinecdm.so")
                     (ld-so (search-input-file
                              inputs #$(glibc-dynamic-linker "x86_64-linux")))
                     (rpath (dirname
                              (search-input-file inputs "/lib/libgcc_s.so.1"))))
                 ;; cannot find section '.interp'. The input file is most likely statically linked
                 ;(invoke "patchelf" "--set-interpreter" ld-so widevine)
                 (invoke "patchelf" "--set-rpath" rpath widevine))))
           (add-after 'build 'create-metadata-files
             (lambda _
               (let* ((major-version #$(version-major version))
                      (major+minor-version #$(version-major+minor version))
                      (minor-version
                        (string-drop major+minor-version
                                     (1+ (string-index major+minor-version #\.)))))
                 (with-output-to-file "manifest.json"
                   (lambda _
                     (format #t "{~@
                             \"name\" : \"WidevineCdm\",~@
                             \"description\": \"Widevine Content Decryption Module\",~@
                             \"version\": \"~a\",~@
                             \"x-cdm-codecs\": \"vp8,vp9.0,avc1,av01\",~@
                             \"x-cdm-host-versions\": \"~a\",~@
                             \"x-cdm-interface-versions\": \"~a\",~@
                             \"x-cdm-module-versions\": \"~a\",~@
                             \"x-cdm-persistent-license-support\": true~@
                             }~%"
                             #$version minor-version minor-version major-version)))
                 (with-output-to-file "widevine.js"
                   (lambda _
                     (format #t
                             "// Set preferences related to widevine loading~@
                             pref(\"media.gmp-widevinecdm.version\", \"~a\");~@
                             pref(\"media.gmp-widevinecdm.visible\", true);~@
                             pref(\"media.gmp-widevinecdm.enabled\", true);~@
                             pref(\"media.gmp-widevinecdm.autoupdate\", false);~@
                             pref(\"media.eme.enabled\", true);~@
                             pref(\"media.eme.encrypted-media-encryption-scheme.enabled\", true);~%"
                             #$version)))
                 ;; This gets symlinked to ~/.config/chromium/WidevineCdm/latest-component-updated-widevine-cdm
                 (with-output-to-file "latest-component-updated-widevine-cdm"
                   (lambda _
                     (format #t "{PATH\":\"~a/lib/chromium\"}~%" %output))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out #$output)
                     (widevine "_platform_specific/linux_x64/libwidevinecdm.so"))
                 (chmod widevine #o755)
                 (install-file "widevine.js"
                               (string-append out "/lib/firefox/browser/defaults/preferences"))
                 (install-file "latest-component-updated-widevine-cdm"
                               (string-append out "/share/chromium"))
                 (install-file "manifest.json"
                               (string-append out "/lib/chromium"))
                 (install-file widevine
                               (string-append out "/lib/chromium"))
                 (install-file widevine
                               (string-append out "/lib/chromium/_platform_specific/linux_x64"))
                 (install-file widevine
                               (string-append out "/lib/qt5/plugins/ppapi"))
                 (install-file widevine
                               (string-append out "/lib/qt6/plugins/ppapi"))))))
         #:substitutable? #f        ; Non-redistributable.
         #:strip-binaries? #f       ; Already stripped?
         #:validate-runpath? #f     ; Fix above `patchelf --set-interpreter` problem first
         #:tests? #f))              ; No tests.
    (native-inputs (list patchelf))
    (inputs (list (list gcc "lib")))
    (home-page "https://www.widevine.com/")
    (synopsis "Browser plugin designed for the viewing of premium video content")
    (description "This package provides a browser plugin designed for the
viewing of premium video content taken from Google Chrome.")
    (supported-systems '("x86_64-linux"))
    (license ((@@ (guix licenses) license)
              "Nonfree"
              "file://LICENSE"
              "This package is non-free"))))

;; https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=widevine-armv7h
;; This package apparently needs patches to glibc
(define-public widevine-armhf
  (package
    (name "widevine")
    (version "4.10.2252.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://archive.raspberrypi.org/debian/"
                            "pool/main/w/widevine/"
                            "libwidevinecdm0_" version "+1_armhf.deb"))
        (sha256
         (base32 "1ikm27ilih0h8vd1jhhp3zxz5mx1vmk4049higr50r04zkymrlzl"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:system "armhf-linux"
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)      ; No configure script.
           (replace 'unpack
             (lambda _
               (invoke "ar" "x" #$source "data.tar.xz")
               (invoke "tar" "xvf" "data.tar.xz")))
           (add-before 'build 'change-directory
             (lambda _
               (chdir "opt/WidevineCdm")))
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((widevine "_platform_specific/linux_arm/libwidevinecdm.so")
                     (ld-so (search-input-file
                              inputs #$(glibc-dynamic-linker "armhf-linux")))
                     (rpath (dirname
                              (search-input-file inputs "/lib/libgcc_s.so.1"))))
                 ;; cannot find section '.interp
                 ;(invoke "patchelf" "--set-interpreter" ld-so widevine)
                 (invoke "patchelf" "--set-rpath" rpath widevine))))
           (add-after 'build 'create-metadata-files
             (lambda _
               (let* ((major-version #$(version-major version))
                      (major+minor-version #$(version-major+minor version))
                      (minor-version
                        (string-drop major+minor-version
                                     (1+ (string-index major+minor-version #\.)))))
                 (with-output-to-file "manifest.json"
                   (lambda _
                     (format #t "{~@
                             \"name\" : \"WidevineCdm\",~@
                             \"description\": \"Widevine Content Decryption Module\",~@
                             \"version\": \"~a\",~@
                             \"x-cdm-codecs\": \"vp8,vp9.0,avc1,av01\",~@
                             \"x-cdm-host-versions\": \"~a\",~@
                             \"x-cdm-interface-versions\": \"~a\",~@
                             \"x-cdm-module-versions\": \"~a\",~@
                             \"x-cdm-persistent-license-support\": true~@
                             }~%"
                             #$version minor-version minor-version major-version)))
                 (with-output-to-file "widevine.js"
                   (lambda _
                     (format #t
                             "// Set preferences related to widevine loading~@
                             pref(\"media.gmp-widevinecdm.version\", \"~a\");~@
                             pref(\"media.gmp-widevinecdm.visible\", true);~@
                             pref(\"media.gmp-widevinecdm.enabled\", true);~@
                             pref(\"media.gmp-widevinecdm.autoupdate\", false);~@
                             pref(\"media.eme.enabled\", true);~@
                             pref(\"media.eme.encrypted-media-encryption-scheme.enabled\", true);~%"
                             #$version)))
                 (with-output-to-file "latest-component-updated-widevine-cdm"
                   (lambda _
                     (format #t "{PATH\":\"~a/lib/chromium\"}~%" %output))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out #$output)
                     (widevine "_platform_specific/linux_arm/libwidevinecdm.so"))
                 (chmod widevine #o755)
                 (install-file widevine
                               (string-append out "/lib/chromium"))))))
         #:substitutable? #f        ; Non-redistributable.
         #:strip-binaries? #f       ; Already stripped?
         #:validate-runpath? #f     ; Fix above `patchelf --set-interpreter` problem first
         #:tests? #f))              ; No tests.
    (native-inputs (list patchelf))
    (inputs (list (list gcc "lib")))
    (home-page "https://www.widevine.com/")
    (synopsis "Browser plugin designed for the viewing of premium video content")
    (description "This package provides a browser plugin designed for the
viewing of premium video content taken from Google Chrome.")
    (supported-systems '("armhf-linux"))
    (license ((@@ (guix licenses) license)
              "Nonfree"
              "file://LICENSE"
              "This package is non-free"))))

;; https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=widevine-aarch64
;; This package apparently needs fixup script applied:
;; https://aur.archlinux.org/cgit/aur.git/tree/widevine_fixup.py?h=widevine-aarch64
;; https://gist.github.com/DavidBuchanan314/c6b97add51b97e4c3ee95dc890f9e3c8
(define widevine-fixup.py
  (origin
    (method url-fetch)
    (uri "https://gist.githubusercontent.com/DavidBuchanan314/c6b97add51b97e4c3ee95dc890f9e3c8/raw/cac73ac13f8e41575c0c680d1ee38bf7c7533d39/widevine_fixup.py")
    (file-name "widevine-fixup.py")
    (sha256
     (base32 "1sghnc89nb0p6b44x4996s4x91a76di4qv06cqv11sr16pidlibq"))))

(define-public widevine-aarch64
  (package
    (name "widevine")
    (version "4.10.2252.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://archive.raspberrypi.org/debian/"
                            "pool/main/w/widevine/"
                            "libwidevinecdm0_" version "+1_arm64.deb"))
        (sha256
         (base32 "1dw36n0zak0dvw64i26qj6y78yc6bbgq6c6162fvcds2xnmjs7zi"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:system "aarch64-linux"
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)      ; No configure script.
           (replace 'unpack
             (lambda _
               (invoke "ar" "x" #$source "data.tar.xz")
               (invoke "tar" "xvf" "data.tar.xz")))
           (add-before 'build 'change-directory
             (lambda _
               (chdir "opt/WidevineCdm")))
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((widevine "_platform_specific/linux_arm64/libwidevinecdm.so")
                     (ld-so (search-input-file
                              inputs #$(glibc-dynamic-linker "aarch64-linux")))
                     (rpath (string-join
                              (list (dirname
                                      (search-input-file inputs "/lib/libgcc_s.so.1"))
                                    (dirname
                                      (search-input-file inputs "/lib/libnspr4.so")))
                              ":")))
                 (rename-file widevine "libwidevinecdm.so")
                 (copy-file #$widevine-fixup.py "widevine-fixup.py")
                 (invoke "python3" "widevine-fixup.py"
                 ;(invoke "python3" #$(this-package-input "widevine-fixup.py")
                         "libwidevinecdm.so" widevine)
                 ;; cannot find section '.interp
                 ;(invoke "patchelf" "--set-interpreter" ld-so widevine)
                 (invoke "patchelf" "--set-rpath" rpath widevine))))
           (add-after 'build 'create-metadata-files
             (lambda _
               (let* ((major-version #$(version-major version))
                      (major+minor-version #$(version-major+minor version))
                      (minor-version
                        (string-drop major+minor-version
                                     (1+ (string-index major+minor-version #\.)))))
                 (with-output-to-file "manifest.json"
                   (lambda _
                     (format #t "{~@
                             \"name\" : \"WidevineCdm\",~@
                             \"description\": \"Widevine Content Decryption Module\",~@
                             \"version\": \"~a\",~@
                             \"x-cdm-codecs\": \"vp8,vp9.0,avc1,av01\",~@
                             \"x-cdm-host-versions\": \"~a\",~@
                             \"x-cdm-interface-versions\": \"~a\",~@
                             \"x-cdm-module-versions\": \"~a\",~@
                             \"x-cdm-persistent-license-support\": true~@
                             }~%"
                             #$version minor-version minor-version major-version)))
                 (with-output-to-file "widevine.js"
                   (lambda _
                     (format #t
                             "// Set preferences related to widevine loading~@
                             pref(\"media.gmp-widevinecdm.version\", \"~a\");~@
                             pref(\"media.gmp-widevinecdm.visible\", true);~@
                             pref(\"media.gmp-widevinecdm.enabled\", true);~@
                             pref(\"media.gmp-widevinecdm.autoupdate\", false);~@
                             pref(\"media.eme.enabled\", true);~@
                             pref(\"media.eme.encrypted-media-encryption-scheme.enabled\", true);~%"
                             #$version)))
                 (with-output-to-file "latest-component-updated-widevine-cdm"
                   (lambda _
                     (format #t "{PATH\":\"~a/lib/chromium\"}~%" %output))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out #$output)
                     (widevine "_platform_specific/linux_arm64/libwidevinecdm.so"))
                 (chmod widevine #o755)
                 (install-file "widevine.js"
                               (string-append out "/lib/firefox/browser/defaults/preferences"))
                 (install-file "latest-component-updated-widevine-cdm"
                               (string-append out "/share/chromium"))
                 (install-file "manifest.json"
                               (string-append out "/lib/chromium"))
                 (install-file widevine
                               (string-append out "/lib/chromium"))
                 (install-file widevine
                               (string-append out "/lib/qt5/plugins/ppapi"))
                 (install-file widevine
                               (string-append out "/lib/qt6/plugins/ppapi"))))))
         #:substitutable? #f        ; Non-redistributable.
         #:strip-binaries? #f       ; Already stripped?
         #:validate-runpath? #f     ; Fix above `patchelf --set-interpreter` problem first
         #:tests? #f))              ; No tests.
    (native-inputs (list nspr patchelf python))
    (inputs (list (list gcc "lib")))
    (home-page "https://www.widevine.com/")
    (synopsis "Browser plugin designed for the viewing of premium video content")
    (description "This package provides a browser plugin designed for the
viewing of premium video content taken from Google Chrome.")
    (supported-systems '("aarch64-linux"))
    (license ((@@ (guix licenses) license)
              "Nonfree"
              "file://LICENSE"
              "This package is non-free"))))

(define* (widevine-for-system #:optional
                              (system (or (%current-target-system)
                                          (%current-system))))
         (cond ((string-prefix? "armhf-" system)
                widevine-armhf)
               ((string-prefix? "aarch64-" system)
                widevine-aarch64)
               (else widevine-x86_64)))

(export widevine-for-system)
