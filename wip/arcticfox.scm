;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip arcticfox)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  )

(define-public arcticfox
  (let ((commit "0be1cbd77b3c44a6f2d7368cec1b818e6778e018") ; Jan 30, 2021
        (revision "1"))
    (package
      (name "arcticfox")
      (version "27.11.0")
      (source
        (origin
          (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/wicknix/Arctic-Fox")
                   (commit commit)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "07mh8apn84ywxsxdncyqrvylgmym8r7sf2lgjwzpaylr79jk8s76"))
            ;(modules '((guix build utils)))
            ;(snippet
            ; '(begin
            ;    (for-each delete-file-recursively
            ;              '(
            ;                ;"db/sqlite3"
            ;                ;"ipc/chromium/src/third_party" ; libevent
            ;                "media/libjpeg"
            ;                "media/libpng"
            ;                ;"media/libvpx"
            ;                ;"media/libwebp"
            ;                "modules/freetype2"
            ;                "modules/libbz2"
            ;                "modules/zlib"
            ;                ;"js/src/ctypes/libffi"
            ;                "security/nss"
            ;                "security/sandbox/chromium-shim/base/third_party"
            ;                ))
            ;    #t))
            ))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f          ; no check target
         #:configure-flags
         (list "--disable-crashreporter"
               "--disable-tests"
               "--disable-debug"
               "--disable-updater"
               "--enable-mozril-geoloc"
               "--disable-webrtc"
               "--disable-safe-browsing"
               "--disable-parental-controls"
               "--enable-release"
               "--disable-necko-wifi"
               "--disable-eme"
               "--disable-gamepad"
               "--enable-dbus"
               ;"--enable-strip"
               ;"--enable-install-strip"
               "--enable-application=browser"
               "--with-branding=browser/branding/arcticfox"
               "--enable-optimize=-O2"

               "--with-distribution-id=org.gnu"
               ;"--enable-default-toolkit=cairo-gtk3"
               "--with-system-bz2"
               "--with-system-jpeg"        ; must be libjpeg-turbo
               ;"--with-system-libevent"    ; seems to be incompatable
               "--with-system-libvpx"
               "--with-system-nspr"
               "--with-system-nss"
               "--with-system-png"
               "--with-system-webp"
               "--with-system-zlib"
               ;"--disable-debug-symbols"
               "--enable-system-cairo"
               "--enable-system-hunspell"
               "--enable-system-pixman"
               "--enable-system-sqlite")
         #:phases
         (modify-phases %standard-phases
           (delete 'bootstrap)
           (replace 'configure
             ;; configure does not work followed by both "SHELL=..." and
             ;; "CONFIG_SHELL=..."; set environment variables instead
             (lambda* (#:key outputs configure-flags #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bash (which "bash"))
                      (abs-srcdir (getcwd))
                      (srcdir (string-append "../" (basename abs-srcdir)))
                      (flags `(,(string-append "--prefix=" out)
                               ,@configure-flags)))
                 ;; Write the configure-flags to .mozconfig for use with ./mach
                 (with-output-to-file ".mozconfig"
                   (lambda _
                     (format #t "export LDFLAGS=\"-Wl,-rpath=~a/lib/arcticfox-~a\"~@
                             mk_add_options MOZ_MAKE_FLAGS=\"-s -j~a\"~@
                             ~{ac_add_options ~a\n~} ~%"
                             out ,version (number->string (parallel-job-count)) flags)))
                 (setenv "SHELL" bash)
                 (setenv "CONFIG_SHELL" bash)
                 (setenv "AUTOCONF" (which "autoconf"))     ; must be autoconf-2.13
                 (setenv "CC" ,(cc-for-target))  ; apparently needed when Stylo is enabled
                 (setenv "MOZ_BUILD_DATE" "20210130000000") ; avoid timestamp
                 (format #t "build directory: ~s~%" (getcwd))
                 (format #t "configure flags: ~s~%" flags))))
           (replace 'build
             (lambda _ (invoke "./mach" "build")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;(invoke "./mach" "package")
                 (invoke "./mach" "install")
                 ;(delete-file-recursively (string-append out "/lib/arcticfox-devel-" ,version))
                 ;(delete-file (string-append out "/bin/arcticfox"))
                 ;(symlink (string-append out "/bin/arcticfox")
                 ;         (string-append out "/lib/arcticfox-" ,version "/arcticfox"))
               #t)))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib"))
                      (gtk (assoc-ref inputs "gtk+"))
                      (gtk-share (string-append gtk "/share"))
                      (mesa (assoc-ref inputs "mesa"))
                      (mesa-lib (string-append mesa "/lib"))
                      ;(pulseaudio (assoc-ref inputs "pulseaudio"))
                      ;(pulseaudio-lib (string-append pulseaudio "/lib"))
                      ;(libxscrnsaver (assoc-ref inputs "libxscrnsaver"))
                      ;(libxscrnsaver-lib (string-append libxscrnsaver "/lib"))
                      )
                 (wrap-program (car (find-files lib "^arcticfox$"))
                   `("XDG_DATA_DIRS" prefix (,gtk-share))
                   ;`("LD_LIBRARY_PATH" prefix (,pulseaudio-lib ,mesa-lib ,libxscrnsaver-lib)))
                   `("LD_LIBRARY_PATH" prefix (,mesa-lib)))
               #t)))
           )
           ))
      (native-inputs
       `(("autoconf" ,((@@ (gnu packages autotools) make-autoconf-wrapper) autoconf-2.13))
         ("automake" ,automake)
         ;("gettext" ,(@ (gnu packages gettext) gettext-minimal))
         ;("libtool" ,libtool)
         ("perl" ,perl)
         ("pkg-config" ,pkg-config)
         ("python" ,python-2)
         ("which" ,(@ (gnu packages base) which))
         ("yasm" ,yasm)))
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ;("bzip2" ,bzip2)
         ("dbus-glib" ,dbus-glib)
         ;("gdk-pixbuf" ,gdk-pixbuf+svg)
         ("glib" ,glib)
         ("gtk" ,gtk+-2)
         ("gtk+" ,gtk+)
         ("hunspell" ,hunspell)
         ("libpng" ,libpng-apng)
         ("libjpeg-turbo" ,libjpeg-turbo)
         ;("libevent" ,libevent)
         ;("libffi" ,libffi)
         ("libvpx" ,libvpx)
         ("libwebp" ,libwebp)
         ("libxt" ,libxt)
         ("mesa" ,mesa)
         ("nspr" ,nspr)
         ("nss" ,nss)
         ("pulseaudio" ,pulseaudio)
         ("sqlite" ,sqlite)
         ("unzip" ,unzip)
         ("zip" ,zip)
         ;("zlib" ,zlib)
         ))
      (home-page "https://github.com/wicknix/Arctic-Fox")
      (synopsis " Web Browser for Mac OS X 10.6+, Windows XP, and PowerPC Linux")
      (description "Arctic Fox aims to be a desktop oriented browser with phone
support removed, or no longer updated in the tree.  The goal here is to
implement specific security updates and bug fixes to keep this browser as up to
date as possible for aging systems. Examples would be Mac OSX 10.6-10.8,
PowerPC's running Linux, Windows XP, etc.  Arctic Fox will build for Mac OS X
10.6 and up, Windows XP, i386/x86_64/PowerPC Linux, and more than likely any
other UNIX/BSD varient.")
      (license license:mpl2.0))))
