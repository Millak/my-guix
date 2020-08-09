;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system python)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xiph))

(define-public mycroft-core
  (package
    (name "mycroft-core")
    (version "20.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/MycroftAI/mycroft-core")
               (commit (string-append "release/v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "076m0rnaq5dg3s891vd2afk4h1fc35c0zdq5i11r0lrr9icls0aa"))))
    (build-system python-build-system)
    (inputs
     `(
       ;; from requirements/requirements.txt
       ("python-six" ,python-six)
       ("python-requests" ,python-requests)
       ("python-gTTS" ,python-gtts)
       ("python-PyAudio", python-pyaudio)
       ("python-pyee" ,python-pyee)
       ("python-SpeechRecognition" ,python-speechrecognition)
       ("python-tornado" ,python-tornado)
       ("python-websocket-client" ,python-websocket-client)
       ("python-requests-futures" ,python-requests-futures)
       ("python-pyserial" ,python-pyserial)
       ("python-psutil" ,python-psutil)
       ;("python-pocketsphinx" ,python-pocketsphinx)
       ("python-inflection" ,python-inflection)
       ("python-pillow" ,python-pillow)
       ("python-dateutil" ,python-dateutil)
       ("python-fasteners" ,python-fasteners)
       ("python-PyYAML" ,python-pyyaml)

       ;("python-lingua-franca" ,python-lingua-franca)
       ;("python-msm" ,python-msm)
       ;("python-msk" ,python-msk)
       ;("python-adapt-parser" ,python-adapt-parser)
       ;("python-padatious" ,python-padatious)
       ;("python-fann2" ,python-fann)
       ;("python-padaos" ,python-padaos)
       ("python-precise-runner" ,python-precise-runner)
       ("python-petact" ,python-petact)
       ("python-pyxdg" ,python-pyxdg-0.26)

       ;; requirements/extra-audiobackend.txt
       ;("python-pychromecast" ,python-pychromecast)
       ;("python-vlc" ,python-vlc)

       ;; requirements/extra-stt.txt
       ;("python-google-api-python-client" ,python-google-api-python-client)
       ))
    (home-page "https://mycroft.ai/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public python-pyxdg-0.26
  (package
    (inherit python-pyxdg)
    (version "0.26")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyxdg" version))
        (sha256
         (base32
          "01hsvbwla1phr2bgqslk35npdzin86sq591jkjrk5v9jyp9jhagy"))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-pyxdg)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'check)))))))

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
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "gTTS" version))
        (sha256
         (base32
          "1zngj2d30pk1bdcni6f9mc991nnyzybdk0a2qx77sxk0nn0fi8ic"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; tests try to connect to the internet
    (propagated-inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-click" ,python-click)
       ("python-gtts-token" ,python-gtts-token)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-six" ,python-six)
       ;("python-testfixtures" ,python-testfixtures)
       ("python-twine" ,python-twine)))
    (home-page "https://github.com/pndurette/gTTS")
    (synopsis
     "gTTS (Google Text-to-Speech), a Python library and CLI tool to interface with Google Translate text-to-speech API")
    (description
     "gTTS (Google Text-to-Speech), a Python library and CLI tool to interface with Google Translate text-to-speech API")
    (license license:expat)))

(define-public python-gtts-token
  (package
    (name "python-gtts-token")
    (version "1.1.3")
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
          "142y1rbyrr74jl3j92v891a0i3vbcyfp3hrsn59c3b3w6mj2nrx9"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f  ; tests try to connect to the internet
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (invoke "python" "-m" "unittest" "discover" "-v" "-s" "gtts_token/tests/")
               #t))))))
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
    (version "7.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyee" version))
        (sha256
         (base32
          "1n5kmqbmjk5xk1yhdz04izns231v2n9s15dqvgvvn619ngnd2269"))))
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
             (if tests?
               (invoke "python" "-m" "unittest" "discover" "--verbose")
               #t))))))
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
    (home-page
     "https://github.com/ross/requests-futures")
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
