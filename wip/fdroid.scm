;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public fdroidserver
  (package
    (name "fdroidserver")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fdroidserver" version))
        (sha256
         (base32
          "1fi4kkgi59gd9nssy9zc86b8rki3m47pd29vc821jgsa89lgjqf5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; this prevents the examples from being installed
         ;; we want to fix this later
         (add-before 'install 'patch-install-locations
           (lambda _
             (substitute* "setup.py"
                          (("\\(data_prefix\\ \\+\\ '/share/doc/fdroidserver/examples',") "")
                          (("\\['buildserver") "'buildserver")
                          (("'\\]\\)") "'")
                          ))))
       #:python ,python-2)) ; after 0.6.0 it switches to python3
    (inputs
     `(("python2-apache-libcloud" ,python2-apache-libcloud)
       ("python2-mwclient" ,python2-mwclient)
       ("python2-paramiko" ,python2-paramiko)
       ("python2-pillow" ,python2-pillow)
       ("python2-pyasn1" ,python2-pyasn1)
       ("python2-pyasn1-modules" ,python2-pyasn1-modules)
       ("python2-pyyaml" ,python2-pyyaml)
       ("python2-requests" ,python2-requests)))
    (propagated-inputs
     `(("python2-crypto" ,python2-crypto))) ; not sure this is the right one
    (home-page "https://f-droid.org")
    (synopsis "F-Droid Server Tools")
    (description "F-Droid Server Tools")
    (license license:agpl3+)))

(define-public python-mwclient
  (package
     (name "python-mwclient")
     (version "0.8.1")
     (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "mwclient" version))
         (sha256
          (base32
           "1r322v6i6xps9xh861rbr4ggshydcgp8cycbdlmgy8qbrh8jg2az"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "py.test")))))))
    (native-inputs
     `(("python-pytest-cache" ,python-pytest-cache)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-pep8" ,python-pytest-pep8)))
    (inputs
     `(("python-funcsigs" ,python-funcsigs)
       ("python-mock" ,python-mock)
       ("python-requests" ,python-requests)
       ("python-responses" ,python-responses)
       ("python-six" ,python-six)))
    (home-page "https://github.com/mwclient/mwclient")
    (synopsis "MediaWiki API client")
    (description "MediaWiki API client")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-mwclient))))))

(define-public python2-mwclient
  (let ((base (package-with-python2
                (strip-python2-variant python-mwclient))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-funcsigs
  (package
    (name "python-funcsigs")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "funcsigs" version))
        (sha256
         (base32
          "0l4g5818ffyfmfs1a924811azhjj8ax9xd1cffr1mzd3ycn0zfx7"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; tests fail to recongnize unittest2
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://funcsigs.readthedocs.org")
    (synopsis
     "Python function signatures from PEP362 for Python 2.6, 2.7 and 3.2+")
    (description
     "Python function signatures from PEP362 for Python 2.6, 2.7 and 3.2+")
    (license license:asl2.0)))

(define-public python2-funcsigs
  (package-with-python2 python-funcsigs))

(define-public python-pytest-pep8
  (package
    (name "python-pytest-pep8")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-pep8" version))
        (sha256
         (base32
          "06032agzhw1i9d9qlhfblnl3dw5hcyxhagn7b120zhrszbjzfbh3"))))
    (build-system python-build-system)
    (inputs
     `(("python-pep8" ,python-pep8)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cache" ,python-pytest-cache)))
    (home-page
     "https://bitbucket.org/pytest-dev/pytest-pep8")
    (synopsis
     "pytest plugin to check PEP8 requirements")
    (description
     "pytest plugin to check PEP8 requirements")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-pytest-pep8))))))

(define-public python2-pytest-pep8
  (let ((base (package-with-python2
                (strip-python2-variant python-pytest-pep8))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-apache-libcloud
  (package
    (name "python-apache-libcloud")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "apache-libcloud" version ".tar.bz2"))
        (sha256
         (base32
          "15fqs5ppkrbky2s4lpq4aif9rki4v18jv9l1j74g88g8qlmh3iwl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'prep-tests
           (lambda _
             ;; this test requires using ssh
             (delete-file "libcloud/test/compute/test_ssh_client.py")
             ;; this is required for the tests to run
             (copy-file "libcloud/test/secrets.py-dist"
                        "libcloud/test/secrets.py"))))))
    (native-inputs
     `(("python-mock" ,python-mock)))
    (home-page "https://libcloud.apache.org/")
    (synopsis
     "A Python library to abstract away differences among cloud provider APIs")
    (description
     "Apache Libcloud is a Python library which hides differences between
different cloud provider APIs and allows you to manage different cloud resources
through a unified and easy to use API.  Resources you can manage with Libcloud
are divided into compute, storage, load balancers, DNS, and container
categories.")
    (license license:asl2.0)))

(define-public python2-apache-libcloud
  (let ((base (package-with-python2 python-apache-libcloud)))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-crypto
  (package
    (name "python-crypto")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "crypto" version))
        (sha256
         (base32
          "1gd9cvw1j5j9nxhi7gla2pggnp678zj7l2dc8n4c2r82d9syjblg"))))
    (build-system python-build-system)
    (inputs
     `(("python-naked" ,python-naked)
       ("python-pyyaml" ,python-pyyaml)
       ("python-shellescape" ,python-shellescape)))
    (home-page "https://github.com/chrissimpkins/crypto")
    (synopsis "Simple symmetric GPG file encryption and decryption")
    (description
     "Simple symmetric GPG file encryption and decryption")
    (license license:expat)))

(define-public python2-crypto
  (let ((base (package-with-python2 python-crypto)))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-shellescape
  (package
    (name "python-shellescape")
    (version "3.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "shellescape" version))
        (sha256
         (base32
          "0n5ky1b2vw2y0d4xl3qybyp2rk0gq5frjs8nr8ak6mgj2fyb4676"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/chrissimpkins/shellescape")
    (synopsis
     "Shell escape a string to safely use it as a token in a shell command (backport of Python shlex.quote for Python versions 2.x & < 3.3)")
    (description
     "Shell escape a string to safely use it as a token in a shell command (backport of Python shlex.quote for Python versions 2.x & < 3.3)")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-shellescape))))))

(define-public python2-shellescape
  (let ((base (package-with-python2
                (strip-python2-variant python-shellescape))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-naked
  (package
    (name "python-naked")
    (version "0.1.31")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Naked" version))
        (sha256
         (base32
          "0zk793hh6z4lirfxz73z4f5yzyx8khhx5w92jh1hfpar2j56pdqj"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-reference
           ;; installing is much easier when you don't depend on yourself
           (lambda _
             (substitute* "setup.py" (("\\['Naked', ") "[")))))))
    (inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)))
    (home-page "http://naked-py.com")
    (synopsis "A command line application framework")
    (description
     "A command line application framework")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-naked))))))

(define-public python2-naked
  (let ((base (package-with-python2 (strip-python2-variant python-naked))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))