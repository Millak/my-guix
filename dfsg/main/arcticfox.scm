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

(define-module (dfsg main arcticfox)
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
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
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
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public arcticfox
  (let ((commit "0be1cbd77b3c44a6f2d7368cec1b818e6778e018") ; Jan 30, 2021
        (revision "1"))
    (package
      (name "arcticfox")
      (version (git-version "27.11.0" revision commit))
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
            (modules '((guix build utils)))
            (snippet
             '(begin
                (for-each delete-file-recursively
                          '(;"db/sqlite3"
                            "gfx/cairo"
                            ;"gfx/graphite2"
                            ;"gfx/harfbuzz"
                            ;"intl/icu"
                            ;"ipc/chromium/src/third_party" ; libevent
                            "media/libjpeg"
                            ;"media/libogg"
                            ;"media/libopus"
                            ;"media/libpng"
                            ;"media/libvpx"
                            ;"media/libwebp"
                            "memory/jemalloc"
                            "modules/freetype2"
                            "modules/libbz2"
                            "modules/zlib"
                            "js/src/ctypes/libffi"
                            "security/nss"
                            "security/sandbox/chromium-shim/base/third_party"))
                #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f          ; check target removed
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
               "--enable-strip"
               "--enable-install-strip"
               "--enable-application=browser"
               "--with-branding=browser/branding/arcticfox"
               "--enable-optimize=-O2"

               "--with-distribution-id=org.gnu"
               "--enable-default-toolkit=cairo-gtk3"
               "--with-system-bz2"
               ;"--with-system-icu"         ; linker error on i686-linux
               "--with-system-jpeg"         ; must be libjpeg-turbo
               ;"--with-system-libevent"    ; seems to be incompatable
               "--with-system-libvpx"
               "--with-system-nspr"
               "--with-system-nss"
               "--with-system-png"
               ;"--with-system-webp"        ; not picked up
               "--with-system-zlib"
               "--disable-debug-symbols"
               "--enable-system-cairo"
               "--enable-system-ffi"
               "--enable-system-hunspell"
               "--enable-system-pixman"
               "--enable-system-sqlite")    ; lacking SQLITE_THREADSAFE?
         #:phases
         (modify-phases %standard-phases
           (delete 'bootstrap)
           (add-after 'unpack 'link-ffmpeg
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((ffmpeg (assoc-ref inputs "ffmpeg")))
                 (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                   (("libavcodec\\.so" all)
                    (string-append ffmpeg "/lib/" all)))
                 #t)))
           (replace 'configure
             ;; configure does not work followed by both "SHELL=..." and
             ;; "CONFIG_SHELL=..."; set environment variables instead
             (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
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
                             out ,(version-major+minor+point version)
                             (number->string (parallel-job-count)) flags)))
                 (setenv "SHELL" bash)
                 (setenv "CONFIG_SHELL" bash)
                 (setenv "AUTOCONF"
                         (string-append (assoc-ref inputs "autoconf")
                                        "/bin/autoconf"))     ; must be autoconf-2.13
                 (setenv "CC" ,(cc-for-target))
                 (setenv "CXX" ,(cxx-for-target))
                 (setenv "MOZ_BUILD_DATE" "20210130000000") ; avoid timestamp
                 (format #t "build directory: ~s~%" (getcwd))
                 (format #t "configure flags: ~s~%" flags)
                 (invoke "./mach" "configure"))))
           (replace 'build
             (lambda _ (invoke "./mach" "build")))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "./mach" "test"))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (invoke "./mach" "install")
               #t)))
           (add-after 'install 'install-desktop-file
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (apps (string-append out "/share/applications")))
                 (mkdir-p apps)
                 (with-output-to-file (string-append apps "/arcticfox.desktop")
                   (lambda _
                     (format #t
                             "[Desktop Entry]~@
                             Name=Arcticfox~@
                             Version=1.0~@
                             Exec=~a/bin/arcticfox %u~@
                             Icon=arcticfox~@
                             Comment=Browse the World Wide Web~@
                             GenericName=Web Browser~@
                             Keywords=Internet;WWW;Browser;Web;Explorer~@
                             Terminal=false~@
                             X-MultipleArgs=false~@
                             Type=Application~@
                             Categories=Network;WebBrowser;~@
                             MimeType=text/html;text/xml;application/xhtml+xml;application/vnd.mozilla.xul+xml;text/mml;x-scheme-handler/http;x-scheme-handler/https;~@
                             StartupNotify=true~@
                             [Desktop Action new-window]~@
                             Name=Open a New Window~@
                             Exec=~@*~a/bin/arcticfox -new-window%"
                             out))))
                        #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib"))
                      (gtk (assoc-ref inputs "gtk+"))
                      (gtk-share (string-append gtk "/share"))
                      (mesa (assoc-ref inputs "mesa"))
                      (mesa-lib (string-append mesa "/lib"))
                      (libpng (assoc-ref inputs "libpng"))
                      (libpng-lib (string-append libpng "/lib"))
                      (pulseaudio (assoc-ref inputs "pulseaudio"))
                      (pulseaudio-lib (string-append pulseaudio "/lib")))
                 (wrap-program (car (find-files lib "^arcticfox$"))
                   `("XDG_DATA_DIRS" prefix (,gtk-share))
                   `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib ,mesa-lib ,libpng-lib)))
               #t))))))
      (native-inputs
       `(("autoconf" ,((@@ (gnu packages autotools)
                           make-autoconf-wrapper) autoconf-2.13))
         ("automake" ,automake)
         ("gcc" ,gcc-6)     ; not gcc-7
         ("perl" ,perl)
         ("pkg-config" ,pkg-config)
         ("python" ,python-2)
         ("unzip" ,unzip)
         ("which" ,(@ (gnu packages base) which))
         ("yasm" ,yasm)
         ("zip" ,zip)))
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ("cairo" ,cairo)
         ("dbus-glib" ,dbus-glib)
         ("ffmpeg" ,ffmpeg)
         ("gdk-pixbuf" ,gdk-pixbuf+svg)
         ("glib" ,glib)
         ("gtk" ,gtk+-2)
         ("gtk+" ,gtk+)
         ("hunspell" ,hunspell)
         ;("icu4c" ,icu4c)
         ("libpng" ,libpng-apng)
         ("libjpeg-turbo" ,libjpeg-turbo)
         ;("libevent" ,libevent)
         ("libffi" ,libffi)
         ("libvpx" ,libvpx)
         ;("libwebp" ,libwebp)
         ("libxt" ,libxt)
         ("mesa" ,mesa)
         ("nspr" ,nspr)
         ("nss" ,nss)
         ("pixman" ,pixman)
         ("pulseaudio" ,pulseaudio)
         ("python" ,python-2)
         ("sqlite" ,sqlite)
         ("zlib" ,zlib)))
      (home-page "https://github.com/wicknix/Arctic-Fox")
      (synopsis "Web Browser for older machines")
      (description "Arctic Fox aims to be a desktop oriented browser for older
devices.  It's goal is to implement specific security updates and bug fixes to
keep this browser as up to date as possible for aging systems.")
      (license license:mpl2.0))))
