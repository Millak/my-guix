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

(define-module (wip dialect)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public dialect
  (package
    (name "dialect")
    (version "1.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dialect-app/dialect")
               (commit version)
               ;; Translations moved to a separate repository
               (recursive? #t)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0nm39prxfbm9yxmih8k4jv2mdbsd3si7bllgh8a30zdym627myhh"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("gtk-update-icon-cache") (which "true"))
               (("update-desktop-database") (which "true")))
             #t))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pylib (string-append out "/lib/python"
                                          ,(version-major+minor
                                             (package-version python))
                                          "/site-packages")))
               (wrap-program (string-append out "/bin/dialect")
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                 `("PYTHONPATH" = (,(getenv "PYTHONPATH") ,pylib))))
             #t)))))
    (inputs
     `(("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("libhandy" ,libhandy)
       ("python-dbus" ,python-dbus)
       ("python-googletrans" ,python-googletrans)
       ("python-gtts" ,python-gtts)
       ("python-pygobject" ,python-pygobject)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/dialect-app/dialect")
    (synopsis "Translation app for GNOME")
    (description "A translation app for GNOME.  It features:
@itemize
@item Translation based on the googletrans Python API, an unofficial API for Google Translate
@item Translation based on the LibreTranslate API, allowing you to use any public instance
@item Translation history
@item Automatic language detection
@item Text to speech
@item Clipboard buttons
@end itemize")
    (license license:gpl3+)))

(define-public python-googletrans
  (package
    (name "python-googletrans")
    (version "3.0.0")
    (source
      (origin
        ;; Tests not included in release
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ssut/py-googletrans")
               ;; tagging releases is HARD!!
               (commit "e10c5e8fa73b32700b0b6eab163e01664b6d5ff1")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "18rrmnyjh2g96qc3nj3mwslr7lpna8irkk0gbaqy8i781vxgnvgq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-version-requirements
           (lambda _
             (substitute* "setup.py"
               (("==0.13.3") ""))
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (when tests?
               ;; Most tests in tests/ fail, want network access.
               (invoke "pytest" "-v" "googletrans" "tests/test_utils.py"))
             #t)))))
    (propagated-inputs
     `(("python-httpx" ,python-httpx)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/ssut/py-googletrans")
    (synopsis "Google Translate API for Python")
    (description
     "Googletrans is a Python library that implements the Google Translate API.  This uses the Google Translate Ajax API to make calls to such methods as detect and translate.")
    (license license:expat)))

(define-public python-gtts
  (package
    (name "python-gtts")
    (version "2.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "gTTS" version))
        (sha256
        (base32 "0g467h1501kxw4zniym03xkz3766bdp6j2j5l04p11ki4h8smkw8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-test-suite
           (lambda _
             ;; This test does use the network.
             (substitute* "gtts/tests/test_tts.py"
               (("def test_bad_fp_type" all)
                (string-append "@pytest.mark.net\n" all)))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-v" "-s" "gtts"
                       "-m" "not net"))
             #t)))))
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)
       ;; Is this needed?
       ("python-gtts-token" ,python-gtts-token)))
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-six" ,python-six)
       ("python-testfixtures" ,python-testfixtures)))
    (home-page "https://github.com/pndurette/gTTS")
    (synopsis "Python library and CLI tool fpr Google Translate text-to-speech API")
    (description
     "This package provides @acronym{gTTS, Google Text-to-Speech}, a Python
library and CLI tool to interface with Google Translate text-to-speech API.")
    (license license:expat)))

(define-public python-gtts-token
  (package
    (name "python-gtts-token")
    (version "1.1.4")
    (source
      (origin
        (method git-fetch)
        ;; tests not included in pypi release
        (uri (git-reference
               (url "https://github.com/Boudewijn26/gTTS-token")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0vr52zc0jqyfvsccl67j1baims3cdx2is1y2lpx2kav9gadkn8hp"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f  ; tests try to connect to the internet
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "unittest" "discover"
                       "-v" "-s" "gtts_token/tests/"))
             #t)))))
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (home-page "https://github.com/boudewijn26/gTTS-token")
    (synopsis "Calculates a token to run the Google Translate text to speech")
    (description "This package provides a Python implementation of the token
validation of Google Translate.")
    (license license:expat)))
