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

(define-module (dfsg main tootle)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config))

(define-public tootle
  (package
    (name "tootle")
    (version "1.0-alpha2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bleakgrey/tootle")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "16xz58xasprza89j3ljrfpgvn05yc00p1ch96nyia99r1dyms9rx"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson/post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "src/Dialogs/NewAccount.vala"
               (("xdg-mime") (which "xdg-mime")))
             ;; Patch for building on glib < 2.64
             (substitute* "src/Build.vala"
               (("(os_name = ).*" _ first) (string-append first "\"GNU\";\n"))
               (("(os_ver = ).*" _ first) (string-append first "\"Guix\";\n"))
               (("GLib.Environment.get_os_info.*") "\"unknown\";\n"))
             #t))
         (add-after 'install 'symlink-package
           (lambda* (#:key outputs #:allow-other-keys)
             (symlink "com.github.bleakgrey.tootle"
                      (string-append (assoc-ref outputs "out") "/bin/tootle"))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")     ; for glib-compile-resources
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib-networking" ,glib-networking)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("libhandy" ,libhandy)
       ("libsoup" ,libsoup)
       ("vala" ,vala-0.50)
       ("xdg-utils" ,xdg-utils)))
    (home-page "https://github.com/bleakgrey/tootle")
    (synopsis "GTK3 client for Mastodon")
    (description "Tootle is a GTK client for Mastodon.  It provides a clean,
native interface that allows you to integrate Mastodon's social experience
seamlessly with your desktop environment.")
    (license license:gpl3+)))
