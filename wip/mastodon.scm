;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages time))

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
                 (url "https://github.com/enkiv2/fern.git")
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
      (propagated-inputs
       `(("mastodon-py" ,python2-mastodon-py)))
      (home-page "https://github.com/enkiv2/fern")
      (synopsis "Curses-based mastodon client")
      (description "Fern is a curses-based mastodon client modeled off usenet
news readers & pine, with an emphasis on getting to 'timeline zero'.")
      (license license:bsd-3))))

(define-public python-mastodon-py
  (package
    (name "python-mastodon-py")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Mastodon.py" version))
        (sha256
         (base32
          "0mypfz5k1phn7b2fk362w8zqh2wi3czf58g4zik64n17r8viww40"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-blurhash" ,python-blurhash)
       ("python-dateutil" ,python-dateutil)
       ("python-decorator" ,python-decorator)
       ("python-magic" ,python-magic)
       ("python-pytz" ,python-pytz)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-blurhash" ,python-blurhash)
       ("python-cryptography" ,python-cryptography)
       ("python-http-ece" ,python-http-ece)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-pytest-vcr" ,python-pytest-vcr)
       ("python-requests-mock" ,python-requests-mock)
       ("python-vcrpy" ,python-vcrpy)))
    (home-page "https://github.com/halcy/Mastodon.py")
    (synopsis "Python wrapper for the Mastodon API")
    (description
     "Python wrapper for the Mastodon API")
    (license license:expat)))

(define-public python2-mastodon-py
  (package-with-python2 python-mastodon-py))

(define-public python-blurhash
  (package
    (name "python-blurhash")
    (version "1.1.4")
    (source
      (origin
        ;; Tests not included in pypi release and releases not tagged in git repo.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/halcy/blurhash-python")
               (commit "22e081ef1c24da1bb5c5eaa2c1d6649724deaef8")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1qq6mhydlp7q3na4kmaq3871h43wh3pyfyxr4b79bia73wjdylxf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (delete-file "setup.cfg")
             (invoke "pytest"))))))
    (native-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/halcy/blurhash-python")
    (synopsis
     "Pure-Python implementation of the blurhash algorithm")
    (description
     "Pure-Python implementation of the blurhash algorithm.")
    (license license:expat)))

(define-public python2-blurhash
  (package-with-python2 python-blurhash))

(define-public python-http-ece
  (package
    (name "python-http-ece")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/web-push-libs/encrypted-content-encoding")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0bp4cc0xc123i72h80ax3qz3ixfwx3j7pw343kc7i6kdvfi8klx7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _ (chdir "python") #t)))))
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-flake8" ,python-flake8)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/web-push-libs/encrypted-content-encoding")
    (synopsis "Encrypted Content Encoding for HTTP")
    (description
     "Encrypted Content Encoding for HTTP.")
    (license license:expat)))

(define-public python2-http-ece
  (package-with-python2 python-http-ece))

(define-public python-pytest-vcr
  (package
    (name "python-pytest-vcr")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ktosiek/pytest-vcr")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1i6fin91mklvbi8jzfiswvwf1m91f43smpj36a17xrzk4gisfs6i"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "tests/"))))))
    (propagated-inputs
     `(("python-pytest" ,python-pytest)
       ("python-vcrpy" ,python-vcrpy)))
    (home-page "https://github.com/ktosiek/pytest-vcr")
    (synopsis "Plugin for managing VCR.py cassettes")
    (description
     "Plugin for managing VCR.py cassettes.")
    (license license:expat)))

(define-public python2-pytest-vcr
  (package-with-python2 python-pytest-vcr))

(define-public tootle
  (package
    (name "tootle")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bleakgrey/tootle.git")
               (commit version)))
               ;(commit "d8fc951fb0c0c0f85a3869c6abfc79b2c3d83215")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1z3wyx316nns6gi7vlvcfmalhvxncmvcmmlgclbv6b6hwl5x2ysi"))))
          ;"14fnxrvlllgnn3f0xzcg82ai2kwhk1lpagr2qr934h9yvh4zqhdn"))))
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
         ;(add-after 'unpack 'dont-use-strict-ssl
         ;  (lambda _
         ;    (substitute* "src/Network.vala"
         ;      (("session.ssl_use_system_ca_file = true")
         ;       "session.ssl_use_system_ca_file = false")
         ;      (("session.ssl_strict = true") "session.ssl_strict = false"))
         ;    #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/com.github.bleakgrey.tootle")
             ;; For GtkFileChooserDialog.
             `("GSETTINGS_SCHEMA_DIR" =
               (,(string-append
                   (assoc-ref outputs "out") ; not gtk+
                   "/share/glib-2.0/schemas")))))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")     ; for glib-compile-resources
       ("pkg-config" ,pkg-config)))
    (inputs
     `(
       ("glib-networking" ,glib-networking)
       ("granite" ,granite)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("libsoup" ,libsoup)
       ("vala" ,vala)))
    (home-page "https://github.com/bleakgrey/tootle")
    (synopsis "GTK3 client for Mastodon")
    (description "Simple Mastodon client designed for elementary OS.")
    (license license:gpl3+)))

(define-public granite
  (package
    (name "granite")
    (version "5.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/elementary/granite.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1gvrk8gh959bmq8w0kaym7sx13v763lk8x5hck00msgmyrsarfwa"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson/post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgee" ,libgee)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/granite")
    (synopsis "Library that extends GTK with common widgets and utilities")
    (description "Granite is a companion library for GTK+ and GLib.  Among other
things, it provides complex widgets and convenience functions designed for use
in apps built for elementary OS.")
    (license license:lgpl3)))

(define-public sweetfish
  (package
    (name "sweetfish")
    (version "0.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/PG-MANA/Sweetfish")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "17gh7mh1vg2cg9kh1irsdyza0qig1y7yn3azdlpx6la9qcfakyrb"))))
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
             #t)))
       #:tests? #f))    ; No test target.
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(;("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)))
    (home-page "https://soft.taprix.org/product/sweetfish.html")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
