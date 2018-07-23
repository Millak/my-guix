;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip fdroid)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg))

(define-public fdroidserver
  (package
    (name "fdroidserver")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fdroidserver" version))
        (sha256
         (base32
          "0cwb1fmindw6v9jkiim9yn3496rk1pvnk94s1r0vz2hxgz16xp7n"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-versioning
           (lambda _
             (substitute* "setup.py"
               (("0.2.1") ,(package-version python-pyasn1-modules)))
             #t)))))
    (propagated-inputs
     `(("python-androguard" ,python-androguard)
       ("python-apache-libcloud" ,python-apache-libcloud)
       ("python-clint" ,python-clint)
       ("python-docker-py" ,python-docker-py-1.10)
       ("python-gitpython" ,python-gitpython)
       ("python-mwclient" ,python-mwclient)
       ("python-paramiko" ,python-paramiko)
       ("python-pillow" ,python-pillow)
       ("python-pyasn1" ,python-pyasn1)
       ("python-pyasn1-modules" ,python-pyasn1-modules)
       ("python-pyyaml" ,python-pyyaml)
       ("python-qrcode" ,python-qrcode)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-requests" ,python-requests)
       ("python-vagrant" ,python-vagrant)))
    (native-inputs
     `(("python-babel" ,python-babel)
       ("python-bcrypt" ,python-bcrypt)
       ("python-docker-pycreds" ,python-docker-pycreds)
       ("python-pynacl" ,python-pynacl)
       ("python-websocket-client" ,python-websocket-client)))
    (home-page "https://f-droid.org")
    (synopsis "F-Droid Server Tools")
    (description "F-Droid Server Tools")
    (license license:agpl3+)))

(define python-docker-py-1.10
  (package
    (inherit python-docker-py)
    (name "python-docker-py")
    (version "1.10.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docker-py" version))
       (sha256
        (base32
         "05f49f6hnl7npmi7kigg0ibqk8s3fhzx1ivvz1kqvlv4ay3paajc"))))))

(define-public python-vagrant
  (package
    (name "python-vagrant")
    (version "0.5.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-vagrant" version))
        (sha256
         (base32
          "1ikrh6canhcxg5y7pzmkcnnydikppv7s6sm9prfx90nk0ac8m6mg"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests involve running vagrant.
    (home-page "https://github.com/todddeluca/python-vagrant")
    (synopsis
     "Python bindings for interacting with Vagrant virtual machines.")
    (description
     "Python bindings for interacting with Vagrant virtual machines.")
    (license license:expat)))

(define-public python-androguard
  (package
    (name "python-androguard")
    (version "3.2.1")
    (source
      (origin
        ;; The pypi release doesn't have the tests, but the tests use
        ;; packaged binaries, so we skip them.
        (method url-fetch)
        (uri (pypi-uri "androguard" version))
        (sha256
         (base32
          "0ndsw00pkyda4i2s3wi5ap8gbk6a9d23xhhxpdbk02padv8sxkfv"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Adapted from .travis.yml
           (lambda _
             (invoke "nosetests" "--with-coverage" "--with-timer"
                     "--timer-top-n" "50"))))))
    (native-inputs
     `(("python-codecov" ,python-codecov)
       ("python-coverage" ,python-coverage)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("python-nose-timer" ,python-nose-timer)))
    (propagated-inputs
     `(("python-asn1crypto" ,python-asn1crypto)
       ("python-colorama" ,python-colorama)
       ("python-future" ,python-future)
       ("python-ipython" ,python-ipython)
       ("python-lxml" ,python-lxml)
       ("python-matplotlib" ,python-matplotlib)
       ("python-networkx" ,python-networkx)
       ("python-pygments" ,python-pygments)
       ("python-pyperclip" ,python-pyperclip)))
    (home-page
     "https://github.com/androguard/androguard")
    (synopsis
     "Androguard is a full python tool to play with Android files.")
    (description
     "Androguard is a full python tool to play with Android files.")
    (license license:asl2.0)))

(define python-asn1crypto-0.24
  (package
    (inherit python-asn1crypto)
    (name "python-asn1crypto")
    (version "0.24.0")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/wbond/asn1crypto.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10lai2cs5mnz3gpaffbw1m7b885ls8328q5wxm35vfmcip1f0xmb"))))))

(define python-codecov
  (package
    (name "python-codecov")
    (version "2.0.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codecov" version))
        (sha256
         (base32
          "1217c0vqf7ii65635gvl27a5pfhv0r7zhrpdp9cx640hg73bgn4f"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-unittest2" ,python-unittest2)))
    (propagated-inputs
     `(("python-coverage" ,python-coverage)
       ("python-requests" ,python-requests)))
    (home-page
     "http://github.com/codecov/codecov-python")
    (synopsis
     "Hosted coverage reports for Github, Bitbucket and Gitlab")
    (description
     "Hosted coverage reports for Github, Bitbucket and Gitlab")
    (license license:asl2.0)))

(define python-pyperclip
  (package
    (name "python-pyperclip")
    (version "1.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyperclip" version))
        (sha256
         (base32
          "1p505c23ji06r28k1y67siihsbdzdf1brhlqpyv9ams4gk9863pp"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Not clear how to make tests pass.
    (inputs
     `(("xclip" ,xclip)
       ("xsel" ,xsel)))
    (home-page
     "https://github.com/asweigart/pyperclip")
    (synopsis
     "Python clipboard module")
    (description
     "A cross-platform clipboard module for Python. (Only handles plain text for now.)")
    (license license:bsd-3)))

(define python-docker-pycreds
  (package
    (name "python-docker-pycreds")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "docker-pycreds" version))
        (sha256
         (base32
          "1zxvam1q22qb0jf48553nnncnfrcp88ag4xa0qmq6vr0imn9a3lb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-versioning
           (lambda _
             (substitute* "test-requirements.txt"
               (("3.0.2") ,(package-version python-pytest))
               (("2.3.1") ,(package-version python-pytest-cov))
               (("2.4.1") ,(package-version python-flake8))
               )
             #t)))))
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page
     "https://github.com/shin-/dockerpy-creds")
    (synopsis
     "Python bindings for the docker credentials store API")
    (description
     "Python bindings for the docker credentials store API")
    (license license:asl2.0)))
