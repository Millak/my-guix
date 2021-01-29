;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip mastodon)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages mastodon)
  #:use-module (gnu packages pantheon)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt))

(define-public fern
  (let ((commit "46067d64ffcc999ce8fe1a4feac76e45b3372438")
        (version "0.0.0")
        (revision "1"))
    (package
      (name "fern")
      (version (git-version version revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/enkiv2/fern")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1f8afjdfd22dygh6mdyf2l69ghgdp45p16v2w3c12ishl460a455"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (setenv "PREFIX" out)
                 (invoke "make" "install"))))
           (delete 'build))))
      (inputs
       `(("mastodon-py" ,python2-mastodon-py)))
      (home-page "https://github.com/enkiv2/fern")
      (synopsis "Curses-based mastodon client")
      (description "Fern is a curses-based mastodon client modeled off usenet
news readers & pine, with an emphasis on getting to 'timeline zero'.")
      (license license:bsd-3))))

;; python-mastodon-py upstreamed in mastodon.scm
(define-public python2-mastodon-py
  (package-with-python2 python-mastodon-py))

;; python-blurhash upstreamed in python-crypto.scm
(define-public python2-blurhash
  (package-with-python2 python-blurhash))

;; python-http-ece upstreamed in python-web.scm
(define-public python2-http-ece
  (package-with-python2 python-http-ece))

;; python-pytest-vcr upstreamed in python-check.scm
(define-public python2-pytest-vcr
  (package-with-python2 python-pytest-vcr))

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
     '(#:phases
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
               (("(os_name =).*" first _)
                "os_name = \"GNU\";\n")
               (("(os_ver =).*" first _)
                "os_ver = \"Guix\";\n")
               (("GLib.Environment.get_os_info.*")
                "\"unknown\";\n"))
             #t))
         ;(add-after 'unpack 'dont-use-strict-ssl
         ;  (lambda _
         ;    (substitute* "src/Services/Network.vala"
         ;      (("session.ssl_use_system_ca_file = true")
         ;       "session.ssl_use_system_ca_file = false")
         ;      (("session.ssl_strict = true") "session.ssl_strict = false"))
         ;    #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (wrap-program (string-append bin "/com.github.bleakgrey.tootle")
               ;; For GtkFileChooserDialog.
               `("GSETTINGS_SCHEMA_DIR" =
                 (,(string-append
                     (assoc-ref outputs "out") ; not gtk+
                     "/share/glib-2.0/schemas"))))
               (rename-file (string-append bin "/com.github.bleakgrey.tootle")
                            (string-append bin "/tootle"))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")     ; for glib-compile-resources
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

(define-public sweetfish
  (package
    (name "sweetfish")
    (version "0.0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/PG-MANA/Sweetfish")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0hc714jyr42sbwd51zarkhmfdighnhjav20bp6xh5wdbk3qfbrkv"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'more-translations
           (lambda _
             (copy-file "src/Translations/en_AU.ts"
                        "src/Translations/en_US.ts")
             (substitute* "src/Translations/CMakeLists.txt"
               (("en_AU.ts") "en_AU.ts en_US.ts"))
             (substitute* "src/Translations/en_US.ts"
               (("en_AU") "en_US"))
             #t))
         ;(add-after 'install 'move-translations
         ;  (lambda* (#:key outputs #:allow-other-keys)
         ;    (let* ((out (assoc-ref outputs "out"))
         ;           (lib (string-append out "/lib"))
         ;           (en_AU (string-append out "/share/locale/en_AU/LC_MESSAGES"))
         ;           (en_US (string-append out "/share/locale/en_US/LC_MESSAGES")))
         ;      (mkdir-p en_AU)
         ;      (mkdir-p en_US)
         ;      (rename-file (string-append lib "/sweetfish/locales/en_AU.qm")
         ;                   (string-append en_AU "/sweetfish.qm"))
         ;      (rename-file (string-append lib "/sweetfish/locales/en_US.qm")
         ;                   (string-append en_US "/sweetfish.qm"))
         ;      (delete-file-recursively lib)
         ;      #t)))
         )
       #:tests? #f))    ; No test target.
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)))
    (home-page "https://soft.taprix.org/product/sweetfish.html")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
