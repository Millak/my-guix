;;; Copyright © 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip mycroft)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages django)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages time)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xiph))

(define-public mycroft-core
  (package
    (name "mycroft-core")
    (version "20.8.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/MycroftAI/mycroft-core")
               (commit (string-append "release/v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0hc1xbxgpi23l77d1avccn4hd31g0q1jz315z24h95w2calww3kz"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; tests try to connect to the internet and expect to play audio
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-loose-package-version-requirements
           (lambda _
             (setenv "MYCROFT_LOOSE_REQUIREMENTS" "TRUE")
             #t))
         (add-after 'unpack 'dont-use-var-tmp
           (lambda _
             (substitute* (find-files "." "\\.py$")
               (("/var/tmp") "/tmp/mycroft"))
             #t))
         (add-before 'build 'set-home
           (lambda _
             (setenv "HOME" (getcwd))
             #t))
         )))
    (inputs
     `(
       ;; from requirements/requirements.txt
       ("python-requests" ,python-requests)
       ("python-gTTS" ,python-gtts)
       ("python-PyAudio", python-pyaudio)
       ("python-pyee" ,python-pyee)
       ("python-SpeechRecognition" ,python-speechrecognition)
       ("python-tornado" ,python-tornado-6)
       ("python-websocket-client" ,python-websocket-client)
       ("python-requests-futures" ,python-requests-futures)
       ("python-pyserial" ,python-pyserial)
       ("python-psutil" ,python-psutil)
       ("python-pocketsphinx" ,python-pocketsphinx)
       ("python-inflection" ,python-inflection)
       ("python-pillow" ,python-pillow)
       ("python-dateutil" ,python-dateutil)
       ("python-fasteners" ,python-fasteners)
       ("python-PyYAML" ,python-pyyaml)

       ("python-lingua-franca" ,python-lingua-franca)
       ("python-msm" ,python-msm)
       ("python-msk" ,python-msk)
       ("python-adapt-parser" ,python-adapt-parser)
       ("python-padatious" ,python-padatious)
       ("python-fann2" ,python-fann2)
       ("python-padaos" ,python-padaos)
       ("python-precise-runner" ,python-precise-runner)
       ("python-petact" ,python-petact)
       ("python-pyxdg" ,python-pyxdg)

       ;; requirements/extra-audiobackend.txt
       ;("python-pychromecast" ,python-pychromecast)
       ;("python-vlc" ,python-vlc)

       ;; requirements/extra-stt.txt
       ;("python-google-api-python-client" ,python-google-api-python-client)
       ))
    (native-inputs
     `(
       ;; requirements/tests.txt
       ("python-coveralls" ,python-coveralls)
       ("python-flake8" ,python-flake8)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-cov-core" ,python-cov-core)
       ("python-sphinx" ,python-sphinx)
       ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)
       ;("python-behave" ,python-behave)
       ;("python-allure-behave" ,python-allure-behave)
       ;("python-vlc" ,python-vlc)
       ))
    (home-page "https://mycroft.ai/")
    (synopsis "Mycroft Core, the Mycroft Artificial Intelligence platform")
    (description "Mycroft Core, the Mycroft Artificial Intelligence platform.")
    (license license:asl2.0)))

(define-public mimic
  (package
    (name "mimic")
    (version "1.3.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/MycroftAI/mimic1")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "000000000000000000000000000000000yfyl90iwq3az120vjsx"))))
    (build-system gnu-build-system)
    (arguments
     `(
       #:configure-flags '("--enable-shared")
       ))
    (native-inputs
     `(
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ))
    (inputs
     `(
       ("pcre2" ,pcre2)
       ("pulseaudio" ,pulseaudio)
       ))
    (home-page "https://mimic.mycroft.ai/")
    (synopsis "Mycroft's TTS engine, based on CMU's Flite")
    (description "Mimic is a fast, lightweight Text-to-speech engine developed
by Mycroft A.I. and VocaliD, based on Carnegie Mellon University’s Flite
(Festival-Lite) software. Mimic takes in text and reads it out loud to create a
high quality voice.")
    (license (list
               #f                       ; lang/vid_gb_ap/*[ch], voices/mycroft_voice_4.0.flitevox; autogenerated files and Popey's voice
               license:expat            ; unittests/cutest.h
               license:public-domain    ; doc/alice
               license:bsd-3            ; lang/cmulex/make_cmulex_helper.py, include/flite_hts_engine.h, src/hts/flite_hts_engine.c, src/hts/hts_engine_API, src/regex/
               license:asl2.0           ; lang/cmu_grapheme_lex/grapheme_unitran_tables.c
               ;license:flite            ; rest
               ))))

(define-public python-petact
  (package
    (name "python-petact")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "petact" version))
        (sha256
         (base32
          "1rjh0fjimmixbvrv6znkfrfa83ndjc4pgyfyl90iwq3az120vjsx"))))
    (build-system python-build-system)
    (home-page "https://github.com/matthewscholefield/petact")
    (synopsis "Package extraction tool")
    (description "A package extraction tool.")
    (license license:expat)))

(define-public python-gtts
  (package
    (name "python-gtts")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "gTTS" version))
        (sha256
         (base32
          "03qah9gxhx8m6apviqyffay2dpijm2k5h88ikzgndyvs6zc18dxm"))))
    (build-system python-build-system)
    (propagated-inputs
     `(;("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-click" ,python-click)
       ;("python-gtts-token" ,python-gtts-token)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-six" ,python-six)
       ("python-testfixtures" ,python-testfixtures)
       ;("python-twine" ,python-twine)
       ))
    (home-page "https://github.com/pndurette/gTTS")
    (synopsis
     "gTTS (Google Text-to-Speech), a Python library and CLI tool to interface with Google Translate text-to-speech API")
    (description
     "gTTS (Google Text-to-Speech), a Python library and CLI tool to interface with Google Translate text-to-speech API")
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
               (invoke "python" "-m" "unittest" "discover" "-v" "-s" "gtts_token/tests/"))
             #t)))))
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (home-page "https://github.com/boudewijn26/gTTS-token")
    (synopsis "Calculates a token to run the Google Translate text to speech")
    (description "This package provides a Python implementation of the token
validation of Google Translate.")
    (license license:expat)))

(define-public python-pyee
  (package
    (name "python-pyee")
    (version "8.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyee" version))
        (sha256
         (base32
          "0cgxbdr4zmil03wwr5fv58789i51gka8a9fxm1dgkf5xs9dwrnlj"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-asyncio" ,python-pytest-asyncio)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-pytest-trio" ,python-pytest-trio)
       ("python-twisted" ,python-twisted)
       ("python-vcversioner" ,python-vcversioner)))
    (home-page "https://github.com/jfhbrook/pyee")
    (synopsis "Port of node.js's EventEmitter to Python")
    (description
     "@code{pyee} supplies a @code{BaseEventEmitter} object that is similar to
the @code{EventEmitter} class from Node.js.  It also supplies a number of
subclasses with added support for async and threaded programming in Python, such
as async/await as seen in Python 3.5+.")
    (license license:expat)))

(define-public python-speechrecognition
  (package
    (name "python-speechrecognition")
    (version "3.8.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Uberi/speech_recognition")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1lq6g4kl3y1b4ch3b6wik7xy743x6pp5iald0jb9zxqgyxy1zsz4"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "third-party")
            (delete-file-recursively "speech_recognition/pocketsphinx-data")
            (for-each delete-file (find-files "speech_recognition" "^flac"))
            #t))))
    (build-system python-build-system)
    (arguments
     ;; alternate tests fail because it wants internet connectivity
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             ;; standard tests fail because there's no attached microphone
             (when tests?
               (invoke "python" "-m" "unittest" "discover" "--verbose"))
             #t)))))
    (propagated-inputs
     `(("python-pyaudio" ,python-pyaudio)))
    (inputs
     `(("flac" ,flac)
       ("pocketsphinx" ,pocketsphinx)
       ("sphinxbase" ,sphinxbase)))
    (home-page "https://github.com/Uberi/speech_recognition")
    (synopsis "Speech recognition module for Python")
    (description "Library for performing speech recognition, with support for
several engines and APIs, online and offline.")
    (license license:bsd-3)))

(define-public python-requests-futures
  (package
    (name "python-requests-futures")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "requests-futures" version))
        (sha256
         (base32
          "0j611g1wkn98qp2b16kqz7lfz29a153jyfm02r3h8n0rpw17am1m"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))    ; Tests require network access.
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (home-page "https://github.com/ross/requests-futures")
    (synopsis "Asynchronous Python HTTP for Humans")
    (description "This package provides a small add-on for the Python requests
http library.")
    (license license:asl2.0)))

(define-public python-precise-runner
  (package
    (name "python-precise-runner")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "precise-runner" version))
        (sha256
         (base32
          "03dqjvw0mafxs5hakhvb3ah8f157n8632a54spss7w2bzc4l4ihs"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyaudio" ,python-pyaudio)))
    ;; 'numpy==1.16',
    ;; 'tensorflow>=1.13,<1.14',  # Must be on piwheels
    ;; 'sonopy',
    ;; 'pyaudio',
    ;; 'keras<=2.1.5',
    ;; 'h5py',
    ;; 'wavio',
    ;; 'typing',
    ;; 'prettyparse>=1.1.0',
    ;; 'precise-runner',
    ;; 'attrs',
    ;; 'fitipy<1.0',
    ;; 'speechpy-fast',
    ;; 'pyache'
    (home-page "https://github.com/MycroftAI/mycroft-precise")
    (synopsis "Wrapper to use Mycroft Precise Wake Word Listener")
    (description
     "Wrapper to use Mycroft Precise Wake Word Listener")
    (license license:asl2.0)))

(define-public python-lingua-franca
  (package
    (name "python-lingua-franca")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lingua_franca" version))
        (sha256
         (base32
          "1b9r5l49hrjdlj5nggmy76s3g2ish0z5lg7a79ma54yh2kzbpljf"))))
        ;; Some files are missing from PyPi.
        ;(method git-fetch)
        ;(uri (git-reference
        ;       (url "https://github.com/MycroftAI/lingua-franca")
        ;       (commit version)))
        ;(file-name (git-file-name name version))
        ;(sha256
        ; (base32
        ;  "000000000000000000000000000000000cg3bdmk0pisymjg7ysx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependency-versions
           (lambda _
             (substitute* "requirements.txt"
               (("==") ">="))
             #t)))))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)))
    (home-page "https://github.com/MycroftAI/lingua-franca")
    (synopsis "Multilingual text parsing and formatting library")
    (description
     "Mycroft's multilingual text parsing and formatting library.")
    (license license:asl2.0)))

(define-public python-msm
  (package
    (name "python-msm")
    (version "0.9.0")
    (source
      (origin
        ;(method url-fetch)
        ;(uri (pypi-uri "msm" version))
        ;(sha256
        ; (base32
        ;  "0dc7mgg3nsd3bqyg1gg0jplldpjl7gyfs09gmzq6k9hah29xbgiz"))))
        ;; Tests not included in release tarball.
        ;; Some files not included in the release tarball.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/MycroftAI/mycroft-skills-manager")
               (commit (string-append "release/v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0n2x3qkzbgk6ycgj5fl3f99dyvk33c85mnjlxbl0p8djrqkwg78l"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f  ; Tests try to access the internet.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (when tests?
               (setenv "HOME" (getcwd))
               (invoke "pytest" "tests"))
             #t)))))
    (propagated-inputs
     `(("python-fasteners" ,python-fasteners)
       ("python-gitpython" ,python-gitpython)
       ("python-lazy" ,python-lazy)
       ("python-pako" ,python-pako)
       ("python-pyxdg" ,python-pyxdg)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/MycroftAI/mycroft-skills-manager")
    (synopsis "Mycroft Skills Manager")
    (description "Mycroft Skills Manager is a command line tool and a Python
module for interacting with the mycroft-skills repository.  It allows querying
the repository for information (skill listings, skill meta data, etc) and of
course installing and removing skills from the system.")
    (license license:asl2.0)))

(define-public python-lazy
  (package
    (name "python-lazy")
    (version "1.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lazy" version ".zip"))
        (sha256
         (base32
          "0hsvbzv92qv0qsq03idwxhvwpb83fjj521ij6mabh3qkmfjjfv9c"))))
    (build-system python-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (home-page "https://github.com/stefanholek/lazy")
    (synopsis "Lazy attributes for Python objects")
    (description
     "Lazy attributes for Python objects")
    (license license:bsd-2)))

(define-public python-pako
  (package
    (name "python-pako")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pako" version))
        (sha256
         (base32 "1izr1ymi94pn0wxsjs1xbbkz9dg2gqcjy4qh3afhc0b73l91rgga"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)))
    (home-page "https://github.com/MycroftAI/pako")
    (synopsis
     "The universal package manager library")
    (description
     "The universal package manager library")
    (license license:asl2.0)))

(define-public python-testfixtures
  (package
    (name "python-testfixtures")
    (version "6.17.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "testfixtures" version))
        (sha256
         (base32
          "1nlv2hz20czjp4a811ichl5kwg99rh84l0mw9wq4rk3idzfs1hsy"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))    ; Lets come back to this later. Need DJANGO_SETTINGS_MODULE
    (native-inputs
     `(("python-django" ,python-django)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ;("python-pytest-django" ,python-pytest-django)
       ("python-sybil" ,python-sybil)
       ("python-twisted" ,python-twisted)
       ("python-zope-component" ,python-zope-component)))
    (home-page "https://github.com/Simplistix/testfixtures")
    (synopsis
      "A collection of helpers and mock objects for unit tests and doc tests.")
    (description
      "A collection of helpers and mock objects for unit tests and doc tests.")
    (license license:expat)))

(define-public python-sybil
  (package
    (name "python-sybil")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sybil" version))
        (sha256
         (base32
          "091mvh08l40yh15008nhkazdqw64r9yyvw1jq4ir42v98vi72zar"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))    ; AttributeError: 'NoneType' object has no attribute 'setup'
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/cjw296/sybil")
    (synopsis
      "Automated testing for the examples in your documentation.")
    (description
      "Automated testing for the examples in your documentation.")
    (license license:expat)))

(define-public python-padaos
  (package
    (name "python-padaos")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "padaos" version))
        (sha256
         (base32
          "0wkd6p3ggf3ffsg3j47fgfcfmmj5k7h5rak88mbkr1r6r35mzh1a"))))
    (build-system python-build-system)
    (home-page "https://github.com/MatthewScholefield/padaos")
    (synopsis
      "A rigid, lightweight, dead-simple intent parser")
    (description
      "A rigid, lightweight, dead-simple intent parser")
    (license license:expat)))

(define-public python-fann2
  (package
    (name "python-fann2")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fann2" version))
        (sha256
         (base32
          "07nlpncl5cx2kzdy3r91g3i1bsnl7n6f7zracwh87q28mmjhmjnd"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-fann-location
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("/usr/pkg/lib")
                (string-append (assoc-ref inputs "fann") "/lib")))
             #t)))))
    (inputs
     `(("fann" ,fann)))
    (native-inputs
     `(("swig" ,swig)))
    (home-page "https://github.com/FutureLinkCorporation/fann2")
    (synopsis
      "Fast Artificial Neural Network Library (FANN) Python bindings.")
    (description
      "Fast Artificial Neural Network Library (FANN) Python bindings.")
    (license license:lgpl2.1)))

(define-public python-padatious
  (package
    (name "python-padatious")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "padatious" version))
        (sha256
         (base32
          "0xbgf75kxclacgairid8m948hrrngcxhykr1wkvav32fp58z4wg4"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-fann2" ,python-fann2)
       ("python-padaos" ,python-padaos)
       ("python-xxhash" ,python-xxhash)))
    (home-page "https://github.com/MycroftAI/padatious")
    (synopsis "A neural network intent parser")
    (description "A neural network intent parser")
    (license #f)))

(define-public python-xxhash
  (package
    (name "python-xxhash")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xxhash" version))
        (sha256
         (base32
          "0g225kk6hj9ab8ggiw5s157jkqh2f2wd8v3g8nhnyiy1aj2q3jjq"))))
    (build-system python-build-system)
    (home-page "https://github.com/ifduyue/python-xxhash")
    (synopsis "Python binding for xxHash")
    (description "Python binding for xxHash")
    (license license:bsd-3)))

(define-public python-adapt-parser
  (package
    (name "python-adapt-parser")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "adapt-parser" version))
        (sha256
         (base32
          "0h9kwdycf4x042xwy7hb9978y917ismx9f3zg55z65iv1ksff17c"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyee" ,python-pyee)
       ("python-six" ,python-six)))
    (home-page "https://github.com/MycroftAI/adapt")
    (synopsis "A text-to-intent parsing framework.")
    (description
      "A text-to-intent parsing framework.")
    (license #f)))

(define-public python-msk
  (package
    (name "python-msk")
    (version "0.3.16")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "msk" version))
        (sha256
         (base32
          "11zf2s5wdglzki2r05plx6j9gykwvbpdn8fbr3fnjz4g0vy1g9y6"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-colorama" ,python-colorama)
       ("python-gitpython" ,python-gitpython)
       ("python-msm" ,python-msm)
       ("python-pygithub" ,python-pygithub)
       ("python-requests" ,python-requests)))
    (home-page "https://github.com/MycroftAI/mycroft-skills-kit")
    (synopsis "Mycroft Skills Kit")
    (description "Mycroft Skills Kit")
    (license #f)))

;; TODO: Unbundle sphinxbase
(define-public python-pocketsphinx
  (package
    (name "python-pocketsphinx")
    (version "0.1.15")
    (source
      (origin
        ;; Not all files included in git repo.
        (method url-fetch)
        (uri (pypi-uri "pocketsphinx" version))
        (sha256
         (base32
          "0qwvix2bq7n2g7kp1kcfa8z3j3yi35f5p0f9rai6zgkxbis91lil"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))    ; No tests included in release tarball.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("pulseaudio" ,pulseaudio)))
    (native-inputs
     `(("swig" ,swig)))
    (home-page "https://github.com/bambocher/pocketsphinx-python")
    (synopsis
      "Python interface to CMU Sphinxbase and Pocketsphinx libraries")
    (description
      "Python interface to CMU Sphinxbase and Pocketsphinx libraries")
    (license license:bsd-3)))
