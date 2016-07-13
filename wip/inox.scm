;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip inox)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl) ; that perl package
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  )

;;; Inspired heavily from the inux in the AUR
;;; https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=inox

(define %chromium_conf
  (list "-Dwerror="
        "-Dclang=0"
        "-Dpython_ver=2.7"
        "-Dlinux_link_gsettings=1"
        "-Dlinux_link_libpci=1"
        "-Dlinux_link_pulseaudio=1"
        "-Dlinux_strip_binary=1"
        "-Dlinux_use_bundled_binutils=0"
        "-Dlinux_use_bundled_gold=0"
        "-Dlinux_use_gold_flags=0"
        "-Dicu_use_data_file_flag=1"
        "-Dlogging_like_official_build=1"
        "-Dffmpeg_branding=Chrome"
        "-Dproprietary_codecs=1"
        "-Duse_gnome_keyring=0"
        "-Duse_system_bzip2=1"
        "-Duse_system_flac=1"
        "-Duse_system_ffmpeg=0"
        "-Duse_system_harfbuzz=1"
        "-Duse_system_icu=0"
        "-Duse_system_libevent=1"
        "-Duse_system_libjpeg=1"
        "-Duse_system_libpng=1"
        "-Duse_system_libvpx=1"
        "-Duse_system_libxml=0"
        "-Duse_system_snappy=1"
        "-Duse_system_xdg_utils=1"
        "-Duse_system_yasm=1"
        "-Duse_system_zlib=0"
        ;"-Dusb_ids_path=/usr/share/hwdata/usb.ids"
        "-Duse_mojo=0"
        "-Duse_gconf=0"
        "-Duse_sysroot=0"
        "-Denable_widevine=1"
        "-Ddisable_fatal_linker_warnings=1"
        "-Ddisable_glibc=1"
        "-Denable_webrtc=0"
        "-Denable_google_now=0"
        "-Denable_remoting=0"
        "-Dsafe_browsing_mode=0"
        "-Denable_rlz=0"
        "-Denable_hangout_services_extension=0"
        "-Dbranding=Chromium"
        "-Dgoogle_chrome_build=0"
        "-Denable_web_speech=1"
        "-Denable_wifi_bootstrapping=0"
        "-Denable_speech_input=0"
        "-Denable_pre_sync_backup=0"
        "-Denable_print_preview=0"
        "-Dtracing_like_official_build=1"
        "-Dfieldtrial_testing_like_official_build=1"
        "-Dfastbuild=1"
        )
  )

(define-public inox
  (package
    (name "inox")
    (version "51.0.2704.79")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://commondatastorage.googleapis.com/chromium-browser-official/chromium-" version ".tar.xz"))
        (sha256
         (base32
          "0dm0hrd2rmrpafxxvsxa67c5ib8n31l7y9gyz1jy91lg2n0dl8r3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((ninja (string-append (assoc-ref inputs "ninja") "/bin/ninja"))
                   (out   (assoc-ref outputs "out")))
               (system* "build/linux/unbundle/replace_gyp_files.py" ,@%chromium_conf)
               (system* "build/gyp_chromium" "--depth=." ,@%chromium_conf)
               (mkdir-p "out/release")
               (system* ninja "-C" "out/release" "chrome" "chrome_sandbox" "chromedriver"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; add more later
               (system* "make" (string-append "PREFIX=" out) install-strip )))))))
    (native-inputs
     `(
       ("gperf" ,gperf)
       ("libvpx" ,libvpx)
       ("mesa" ,mesa)
       ("ninja" ,ninja)
       ("python-2" ,python-2)
       ("yasm" ,yasm)
       ))
    (inputs
     `(
       ("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("dbus" ,dbus)
       ("flac" ,flac)
       ("gtk-2" ,gtk+-2)
       ("harfbuzz" ,harfbuzz)
       ("libevent" ,libevent)
       ("libexif" ,libexif)
       ("libgcrypt" ,libgcrypt)
       ("libvpx" ,libvpx)
       ("nss-certs" ,nss-certs) ; or nss
       ("pciutils" ,pciutils)
       ("perl" ,perl)
       ("perl-file-basedir" ,perl-file-basedir)
       ("pulseaudio" ,pulseaudio)
       ("xdg-utils" ,xdg-utils)
       ;("libxxs" ,libxxs)
       ))
    (home-page "https://www.chromium.org/")
    (synopsis "Chromium web broswer")
    (description
     "Chromium is an open-source browser project that aims to build a safer,
faster, and more stable way for all Internet users to experience the web.")
    (license license:bsd-3)))

(define-public perl-file-basedir
  (package
    (name "perl-file-basedir")
    (version "0.07")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/K/KI/KIMRYAN/File-BaseDir-"
               version ".tar.gz"))
        (sha256
         (base32
          "0aq8d4hsaxqibp36f773y6dfck7zd82v85sp8vhi6pjkg3pmf2hj"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-which" ,perl-file-which)
       ("perl-module-build" ,perl-module-build)))
    (inputs
     `(("perl-ipc-system-simple" ,perl-ipc-system-simple)))
    (home-page "http://search.cpan.org/dist/File-BaseDir")
    (synopsis "Use the Freedesktop.org base directory specification")
    (description "fill-in-yourself!")
    (license (package-license perl))))
