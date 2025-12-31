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

(define-module (wip microblog-pub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time))

(define-public microblog-pub
  (package
    (name "microblog-pub")
    (version "2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.sr.ht/~tsileo/microblog.pub")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "193w448cynjhxy73w0p8w1548m0c8iiycrj7jv8c2aidazq16aa8"))))
    (build-system pyproject-build-system)
    (inputs
     (list
       python-aiosqlite
       python-alembic
       python-asgiref
       python-bcrypt
       python-bleach
       python-blurhash
       python-boussole
       python-brotli
       python-beautifulsoup4
       python-cachetools
       python-dateutil
       python-emoji
       python-fastapi
       ;python-feedgen
       python-greenlet
       python-humanize
       python-invoke
       python-itsdangerous
       python-jinja2
       python-html2text
       python-html5lib
       python-httpx
       python-loguru
       python-mf2py
       python-mistletoe
       python-multipart
       python-pillow
       python-pebble
       python-prompt-toolkit
       python-pycryptodome
       python-pygments
       python-pyld
       python-sqlalchemy
       ;python-supervisor
       python-tabulate
       python-tomli
       python-tomli-w
       python-uvicorn
       ))
    (native-inputs
     (list
       poetry
       python-poetry-core

       python-black
       python-boussole
       python-flake8
       python-invoke
       python-isort
       python-factory-boy
       python-libsass
       python-mypy
       python-pytest
       python-pytest-asyncio
       ;python-respx
       ;python-sqlalchemy2-stubs
       ;python-types-bleach
       ;python-types-cachetools
       ;python-types-dateutil
       ;python-types-emoji
       ;python-types-markdown
       ;python-types-pillow
       ;python-types-requests
       ;python-types-tabulate
       ))
    (home-page "https://sr.ht/~tsileo/microblog.pub/")
    (synopsis "Self-hosted, single-user, ActivityPub powered microblog")
    (description "mircroblog.pub is a self-hosted, single-user, ActivityPub
powered microblog.")
    (license license:agpl3)))

;; Tests not included in pypi release
(define-public python-boussole
  (package
    (name "python-boussole")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "boussole" version))
              (sha256
               (base32
                "0bqs16fhlqz17n0xfyafsnblrmdqfxmb2wcxcrwcffc3d6073474"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-click
           python-colorama
           python-colorlog
           python-libsass
           python-pyaml
           python-watchdog))
    (native-inputs
     (list python-flake8
           python-livereload
           python-packaging
           python-pytest
           python-sphinx
           python-sphinx-rtd-theme
           python-twine))
    (home-page "https://github.com/sveetch/boussole")
    (synopsis
     "Commandline interface to build Sass projects using libsass-python")
    (description
     "Commandline interface to build Sass projects using libsass-python")
    (license license:expat)))
