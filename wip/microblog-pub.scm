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
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time))

(define-public microblog-pub
  (package
    (name "microblog-pub")
    (version "2.0.0-rc.11")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.sr.ht/~tsileo/microblog.pub")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "14rwghxjvak5f89nbcgpymcaakpnsnvdyi209n2yramf1z0jvmsb"))))
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
       ;python-fastapi
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

(define-public python-loguru
  (package
    (name "python-loguru")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "loguru" version))
              (sha256
               (base32
                "076l16ilgdb0pjbbkx21d39kzysvlyswdnbghgli79fhb1kx0sq6"))))
    (build-system python-build-system)
    (native-inputs
     (list python-black
           python-colorama
           python-docutils
           python-flake8
           python-isort
           python-pytest
           python-pytest-cov
           python-sphinx
           python-sphinx-autobuild
           python-sphinx-rtd-theme
           python-tox))
    (home-page "https://github.com/Delgan/loguru")
    (synopsis "Python logging made (stupidly) simple")
    (description "Python logging made (stupidly) simple")
    (license license:expat)))

;; needs the hatchling pyproject build backend
(define-public python-fastapi
  (package
    (name "python-fastapi")
    (version "0.89.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "fastapi" version))
              (sha256
               (base32
                "03sqd8vhrivy9ynk999jhpfl2hnfw5r5rbm2bh0jlmrbwlg2gn8m"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list
       python-starlette
       python-pydantic
       ))
    (home-page "https://fastapi.tiangolo.com/")
    (synopsis
      "FastAPI framework, high performance, easy to learn, fast to code, ready for production")
    (description
      "FastAPI framework, high performance, easy to learn, fast to code, ready for
      production")
      (license license:expat)))
