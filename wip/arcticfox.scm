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
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
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
            (modules '((guix build utils)))
            (snippet
             '(begin
                (for-each delete-file-recursively
                          '(
                            "db/sqlite3"
                            "ipc/chromium/src/third_party"
                            "modules/freetype2"
                            "js/src/ctypes/libffi"
                            ;"intl/icu"
                            "security/nss"
                            ))
                #t))
            ))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f          ; no check target
         #:out-of-source? #t  ; must be built outside of the source directory
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
               ;"--disable-gio"
               ;"--disable-pulseaudio"
               ;"--enable-strip"
               ;"--enable-install-strip"
               "--enable-application=browser"
               "--with-branding=browser/branding/arcticfox"
               "--enable-optimize=-O2"

               "--with-distribution-id=org.gnu"
               "--disable-debug-symbols"
               "--with-system-jpeg"        ; must be libjpeg-turbo
               "--with-system-nspr"
               "--with-system-nss"
               ;"--with-system-icu"
               "--with-system-libevent"
               "--with-system-zlib"
               "--with-system-bz2"
               "--with-system-webp"
               "--with-system-cairo"
               "--with-system-sqlite"
               ;; UNBUNDLE-ME! "--with-system-ogg"
               ;; UNBUNDLE-ME! "--with-system-vorbis"
               ;; UNBUNDLE-ME! "--with-system-theora" ; wants theora-1.2, not yet released
               "--with-system-libvpx"
               ;; UNBUNDLE-ME! "--with-system-harfbuzz"
               ;; UNBUNDLE-ME! "--with-system-graphite2"
               "--enable-system-pixman"
               "--enable-system-ffi"
               )
         #:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (substitute* (find-files "build/autoconf")
                 (("#!/bin/sh") (string-append "#!" (which "sh")))
                 (("#! /bin/sh") (string-append "#! " (which "sh"))))
               (invoke "autoreconf" "--verbose" "--force" "-i")))
           (replace 'configure
             ;; configure does not work followed by both "SHELL=..." and
             ;; "CONFIG_SHELL=..."; set environment variables instead
             (lambda* (#:key outputs configure-flags #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bash (which "bash"))
                      (abs-srcdir (getcwd))
                      (srcdir (string-append "../" (basename abs-srcdir)))
                      (flags `(,(string-append "--prefix=" out)
                               ;,(string-append "--with-l10n-base="
                               ;                abs-srcdir "/l10n")
                               ,@configure-flags)))
                 (setenv "SHELL" bash)
                 (setenv "CONFIG_SHELL" bash)
                 (setenv "_CONFIG_SHELL" bash)
                 (setenv "AUTOCONF" (which "autoconf")) ; must be autoconf-2.13
                 (setenv "CC" ,(cc-for-target))  ; apparently needed when Stylo is enabled
                 (setenv "MOZ_BUILD_DATE" "20210130000000") ; avoid timestamp
                 (mkdir "../build")
                 (chdir "../build")
                 (format #t "build directory: ~s~%" (getcwd))
                 (format #t "configure flags: ~s~%" flags)
                 (apply invoke bash
                        (string-append srcdir "/configure")
                        flags)
                 ;(symlink "mozcfg-amd64linux" ".mozconfig")
                 )))
           (replace 'build
             (lambda _ (invoke "./mach" "build")))
           (replace 'install
             (lambda _ (invoke "./mach" "install")))
           )))
      (native-inputs
       `(
         ("autoconf" ,autoconf-2.13)
         ("automake" ,automake)
         ("gettext" ,(@ (gnu packages gettext) gettext-minimal))
         ("libtool" ,libtool)
         ;("m4" ,(@ (gnu packages m4) m4))
         ("perl" ,perl)
         ("pkg-config" ,pkg-config)
         ("python" ,python-2)
         ("which" ,(@ (gnu packages base) which))
         ("yasm" ,yasm)
         ))
      (inputs
       `(
         ("alsa-lib" ,alsa-lib)
         ("bzip2" ,bzip2)
         ("dbus-glib" ,dbus-glib)
         ("gdk-pixbuf" ,gdk-pixbuf)
         ("glib" ,glib)
         ("gtk" ,gtk+-2)
         ;("icu4c" ,icu4c)
         ("libjpeg-turbo" ,libjpeg-turbo)
         ("libevent" ,libevent)
         ("libffi" ,libffi)
         ("libvpx" ,libvpx)
         ("libwebp" ,libwebp)
         ("libxt" ,libxt)
         ("nspr" ,nspr)
         ("nss" ,nss)
         ("pango" ,pango)
         ("pulseaudio" ,pulseaudio)
         ("unzip" ,unzip)
         ("zip" ,zip)
         ("zlib" ,zlib)
         ))
      (home-page "https://github.com/wicknix/Arctic-Fox")
      (synopsis "")
      (description "")
      (license license:mpl2.0))))
