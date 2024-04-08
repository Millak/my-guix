;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip clangen)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sdl))

;; TODO: Replace fonts in resources/fonts, mostly notosans
;; Needs a newer poetry
(define-public clangen
  (package
    (name "clangen")
    (version "0.11.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ClanGenOfficial/clangen")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qnzqa7s3r2ji52jdh5kpi75rnylx7a373l60k3qdzis0j19y4dd"))
        ))
    (build-system pyproject-build-system)
    (arguments
     (list #:build-backend "poetry"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'write-version-ini
                 (lambda _
                   (with-output-to-file "version.ini"
                     (lambda () (string-append "[DEFAULT]\n"
                                               "version_number=~a\n"
                                               "upstream=GNU Guix\n")
                       #$(package-version this-package)))))
               (replace 'build
                 (lambda _
                   (invoke "poetry" "run"
                           "python3" "-m" "PyInstaller"
                           "Clangen.spec"))))))
    (inputs
     (list python-pgpy
           python-platformdirs
           python-pygame-ce
           python-pygame-gui
           python-pyinstaller
           ;python-pypresence   ; discord?
           python-requests
           python-strenum
           python-ujson))
    (native-inputs
     (list poetry))
    (home-page "https://clangen.io/")
    (synopsis "Warrior Cats fan game")
    (description "Clangen is a fan-edit of the Warrior Cat Clangen game.")
    (license license:mpl2.0)))

(define-public python-pygame-ce
  (package
    (name "python-pygame-ce")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pygame-ce" version))
       (sha256
        (base32 "1w4wvg3ag54cihnabahbnypbkq9fr5fzlkvmsq7klqvw86hlma3h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'dont-target-AVX2
             (lambda _
               (substitute* "setup.py"
                 (("should_use_avx2 = True") "should_use_avx2 = False")))))))
    (native-inputs
     (list pkg-config python-cython))
    (inputs
     (list freetype
           (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))))
    (home-page "https://pyga.me")
    (synopsis "Python Game Development")
    (description "Pygame is a library for the development of multimedia
applications like video games using Python. It uses the SDL library and several
other popular libraries to abstract the most common functions, making writing
these programs a more intuitive task.")
    (properties `((tunable? . #t)))
    (license license:lgpl2.1)))

;; TODO: Unbundle fonts from pygame_gui/data
(define-public python-pygame-gui
  (package
    (name "python-pygame-gui")
    (version "0.6.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/MyreMylar/pygame_gui")
               (commit (string-append "v_" (string-delete #\. version)))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0s0i7rxw9fs451c9flakzsbzdyq85v33rjxq9ggxd89m9g8k8x91"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'check 'pre-check
             (lambda _
               (setenv "HOME" (getcwd)))))))
    (propagated-inputs
     (list python-i18n
           python-pygame-ce))
    (native-inputs (list python-pytest python-pytest-benchmark))
    (home-page "https://github.com/MyreMylar/pygame_gui")
    (synopsis "GUI module for pygame Community Edition")
    (description "Helps create GUIs for games made using pygame Community
Edition.  Features HTML-style text formatting, localization, theme files to
control the look and a system to manage multiple windows of GUI stuff.")
    (license license:expat)))

(define-public python-i18n
  (package
    (name "python-i18n")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-i18n" version))
       (sha256
        (base32 "1s74f7sgay30kj80pqx9aa74d0slwklfzjynzgmsgwsb6v9g75yz"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ; Tests not included.
    (propagated-inputs
     (list python-pyyaml))
    (home-page "https://github.com/tuvistavie/python-i18n")
    (synopsis "Translation library for Python")
    (description "This package provides a translation library for Python.")
    (license license:expat)))

(define-public python-pyinstaller
  (package
    (name "python-pyinstaller")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyinstaller" version))
       (sha256
        (base32 "1vrqr23n6amgsd0p629bmf2x3nk5lsymhhkd98yvyi1k1z34jgcg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'check 'pre-check
             (lambda _
               (substitute* "tests/functional/test_misc.py"
                 (("/bin/sh") (which "sh"))))))
       #:test-flags
       #~(list "-k"
               (string-append "not test_automatic_reclassification_binary[onedir]"
                              " and not test_pyi_splash[notkinter-onedir]"
                              " and not test_pyi_splash[notkinter-onefile]"
                              " and not test_pyi_splash[tkinter-onedir]"
                              " and not test_pyi_splash[tkinter-onefile]"
                              " and not test_ldconfig_cache"
                              " and not test_metadata_searching"))))
    (propagated-inputs (list python-altgraph python-packaging
                             python-pyinstaller-hooks-contrib
                             python-setuptools))
    (inputs (list zlib))
    (native-inputs (list python-execnet python-psutil python-pytest))
    (home-page "https://www.pyinstaller.org/")
    (synopsis
     "Bundle a Python application and all dependencies into a single package")
    (description
     "@code{PyInstaller} bundles a Python application and all its dependencies
into a single package.")
    ;; GPL-2.0-or-later WITH Bootloader-exception, embeddable bits under Apache2.
    (license (list license:gpl2+ license:asl2.0 license:expat))))

(define-public python-pyinstaller-hooks-contrib
  (package
    (name "python-pyinstaller-hooks-contrib")
    (version "2023.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyinstaller-hooks-contrib" version))
       (sha256
        (base32 "0zz7kc9wv83hqnq190nii1glvx3npv7sn71qmb6ijp56ajhaimsx"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))    ; No tests.
    (home-page "https://github.com/pyinstaller/pyinstaller-hooks-contrib")
    (synopsis "Community maintained hooks for PyInstaller")
    (description "Community maintained hooks for @code{PyInstaller}")
    (license license:gpl2+)))
