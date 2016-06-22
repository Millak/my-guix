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

(define-module (packages onionshare)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages zip))

(define-public onionshare
  (package
    (name "onionshare")
    (version "0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/micahflee/onionshare/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0pc3xbq379415s0i0y6rz02hay20zbvgra1jmg4mgrl9vbdr8zmw"))))
    (build-system python-build-system)
    (inputs
     `(("python-flask" ,python-flask)
       ("python-nautilus" ,python-nautilus)
       ("python-stem" ,python-stem)
       ("python-pyqt" ,python-pyqt)))
    (home-page "https://onionshare.org/")
    (synopsis "Securely and anonymously share files")
    (description "OnionShare lets you securely and anonymously share files of
any size.  It works by starting a web server, making it accessible as a Tor
hidden service, and generating an unguessable URL to access and download the
files.  It doesn't require setting up a server on the internet somewhere or
using a third party filesharing service.  You host the file on your own computer
and use a Tor hidden service to make it temporarily accessible over the
internet.  The other user just needs to use Tor Browser to download the file
from you.")
    (license license:gpl3+)))

(define-public python-nautilus
  (package
    (name "python-nautilus")
    (version "0.4.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "5c/9c/ed9015ffc10219e70bf01a33c912c3fbd1400d356c0e6aa606cec4b0932b"
               "/nautilus-" version ".tar.gz"))
        (sha256
         (base32
          "01hwzjc1zshk4vvxrcghm398fpy4jls66dyz06g07mrwqif8878p"))))
  (build-system python-build-system)
  (arguments `(#:tests? #f)) ; todo re-enable
  (native-inputs
   `(
     ("python-graphql-core" ,python-graphql-core)
     ("python-graphql-relay" ,python-graphql-relay)
     ("python-pycparser" ,python-pycparser)
     ("python-requests" ,python-requests)
     ("python-setuptools" ,python-setuptools)
     ))
  (inputs
   `(
     ("python-bcrypt" ,python-bcrypt)
     ("python-click" ,python-click)
     ("python-consul" ,python-consul)
     ("python-graphene" ,python-graphene)
     ("python-jinja2" ,python-jinja2)
     ("python-nose2" ,python-nose2)
     ("python-peewee" ,python-peewee)
     ("python-pika" ,python-pika)
     ("python-tornado" ,python-tornado)
     ("python-wtforms" ,python-wtforms)))
    (home-page "https://github.com/AlecAivazis/nautilus")
    (synopsis "A library for creating microservice applications")
    (description
     "Nautilus is a framework for flux based microservices that looks to
provide extendible implementations of common aspects of a cloud so that you can
focus on building massively scalable web applications.")
    (license license:expat)))

(define-public python-bcrypt
  (package
    (name "python-bcrypt")
    (version "2.0.0")
    (source
      (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/"
             "11/7d/4c7980d04314466de42ea804db71995c9b3a2a47dc79a63c51f1be0cfd50"
             "/bcrypt-" version ".tar.gz"))
      (sha256
       (base32
        "1yl78fnkyxkg6vbas7lsmrrsknhh7fpygp958svhxl90y9z1jbcb"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pycparser" ,python-pycparser)
       ("python-pytest" ,python-pytest)))
    (inputs
     `(("python-cffi" ,python-cffi)
       ("python-six" ,python-six)))
    (home-page "https://github.com/pyca/bcrypt/")
    (synopsis
     "Modern password hashing library")
    (description
     "bcrypt is a Python module which provides a password hashing method based
on the Blowfish password hashing algorithm, as described in
@url{http://static.usenix.org/events/usenix99/provos.html,\"A Future-Adaptable
Password Scheme\"} by Niels Provos and David Mazieres.")
    (license license:asl2.0)))

(define-public python2-bcrypt
  (let ((bcrypt (package-with-python2 python-bcrypt)))
    (package (inherit bcrypt)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs bcrypt))))))

(define-public python-stem
  (package
    (name "python-stem")
    (version "1.4.1b")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "58/d1/aa1f9cd1f8b5a8a0def0eebd8e58df8d0b98f904a2403f3eedf92f47fbda"
               "/stem-" version ".tar.bz2"))
        (sha256
         (base32
          "09a3amp1y351nwz088ckiibbp666qi2lxwkyknavswfm400s0ns7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero? (system* "python" "run_tests.py" "--unit")))))))
    (inputs
     `(("python-mock" ,python-mock)
       ("python-pep8" ,python-pep8)
       ("python-pyflakes" ,python-pyflakes)
       ("python-pycrypto" ,python-pycrypto)))
    (home-page "https://stem.torproject.org/")
    (synopsis "Controller library for Tor")
    (description
     "Stem is a Python controller library that allows applications to interact
with Tor (https://www.torproject.org/).  With it you can use Tor's control
protocol to script against the Tor process.")
    (license license:gpl3)))

(define-public python2-stem
  (package-with-python2 python-stem))

(define-public python-nose2
(package
  (name "python-nose2")
  (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "cc/12/2f5257e2aaaf8fbf752a2da34faed4dcc49784581daf47a7045d07a6cf10"
               "/nose2-" version ".tar.gz"))
        (sha256
         (base32
          "0wv33xpg4l9baslidl3i5d8z043gi9yi18k3dl79ahfn0pp7740i"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; TODO: re-enable
    (native-inputs
     `(("python-pytest-cov" ,python-pytest-cov)))
    (inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/nose-devs/nose2")
    (synopsis "nose2 is the next generation of nicer testing for Python")
    (description
     "nose2 is the next generation of nicer testing for Python")
    (license license:bsd-2)))

(define-public python2-nose2
  (package-with-python2 python-nose2))

(define-public python-cov-core
  (package
      (name "python-cov-core")
        (version "1.15.0")
          (source
                (origin
                        (method url-fetch)
                              (uri (string-append
                                                  "https://pypi.python.org/packages/4b/87/13e75a47b4ba1be06f29f6d807ca99638bedc6b57fa491cd3de891ca2923/cov-core-"
                                                               version
                                                                            ".tar.gz"))
                                    (sha256
                                              (base32
                                                          "0k3np9ymh06yv1ib96sb6wfsxjkqhmik8qfsn119vnhga9ywc52a"))))
            (build-system python-build-system)
              (inputs
                    `(("python-coverage" ,python-coverage)))
                (home-page
                      "https://github.com/schlamar/cov-core")
                  (synopsis
                        "plugin core for use by pytest-cov, nose-cov and nose2-cov")
                    (description
                          "plugin core for use by pytest-cov, nose-cov and nose2-cov")
                      (license #f)))

(define-public python-consul
  (package
    (name "python-consul")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "1d/75/17bfc3aa64a4be11ef3a5e8211f8177a44d6a5cd323b64f8d451e9d59b71"
               "/python-consul-" version ".tar.gz"))
        (sha256
         (base32
          "0vfyr499sbc4nnhhijp2lznyj507nnak95bvv9w8y78ngxggskbh"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (home-page "https://github.com/cablehead/python-consul")
    (synopsis "Python client for Consul (http://www.consul.io/)")
    (description
     "Python client for Consul (http://www.consul.io/)")
    (license license:expat)))

(define-public python2-consul
  (let ((consul (package-with-python2 python-consul)))
    (package (inherit consul)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs consul))))))

(define-public python-graphene
  (package
    (name "python-graphene")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "f8/62/0c692b804fde05c5b94eede5912dbde05175fe5873d2d5a748a7919c9304"
               "/graphene-" version ".tar.gz"))
        (sha256
         (base32
          "09zhac7igh9ixdz0ay6csy35b40l1jwbf2wrbxmgxwfhy51iy06q"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-django-filter" ,python-django-filter)
       ("python-mock" ,python-mock)
       ("python-psycopg2" ,python-psycopg2)
       ("python-pytest-django" ,python-pytest-django)
       ("python-sqlalchemy-utils" ,python-sqlalchemy-utils)))
    (inputs
     `(("python-graphql-core" ,python-graphql-core)
       ("python-graphql-relay" ,python-graphql-relay)
       ("python-iso8601" ,python-iso8601)
       ("python-promise" ,python-promise)
       ("python-six" ,python-six)))
    (home-page "http://graphene-python.org/")
    (synopsis "GraphQL Framework for Python")
    (description
     "Graphene is a Python library for building GraphQL schemas/types.
A GraphQL schema describes your data model, and provides a GraphQL server
with an associated set of resolve methods that know how to fetch data.")
    (properties `((python2-variant . ,(delay python2-graphene))))
    (license license:expat)))

(define-public python2-graphene
  (let ((base (package-with-python2
                (strip-python2-variant python-graphene))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ("python2-sqlalchemy" ,python2-sqlalchemy)
         ,@(package-native-inputs base))))))

(define-public python-django-filter
  (package
    (name "python-django-filter")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "04/8e/e8a694b9f7894daa41b8a5b014ec473784865d3a79d34e2a908120f28b4a"
               "/django-filter-" version ".tar.gz"))
        (sha256
         (base32
          "1z7h640zrm7m4vgv709iv62c1jzzmq6113yhj4d6ssgyfr2n3hdl"))))
    (build-system python-build-system)
    (home-page "https://django-filter.readthedocs.io/en/latest/")
    (synopsis "Reusable Django application to filter querysets dynamically")
    (description
     "Django-filter is a generic, reusable application to alleviate writing
some of the more mundane bits of view code.  Specifically, it allows users to
filter down a queryset based on a model’s fields, displaying the form to let
them do this.")
    (properties `((python2-variant . ,(delay python2-django-filter))))
    (license license:bsd-3)))

(define-public python2-django-filter
  (let ((base (package-with-python2
                (strip-python2-variant python-django-filter))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-pytest-django
  (package
    (name "python-pytest-django")
    (version "2.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "50/6d/77a4644d15746ed2a243ed557af693cec7887e43b357919ba0b4fd029518"
               "/pytest-django-" version ".tar.gz"))
        (sha256
         (base32
          "1mmc7zsz3dlhs6sx4sppkj1vgshabi362r1a8b8wpj1qfximpqcb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-setuppy
           (lambda _
             (substitute* "setup.py"
                          (("setuptools_scm==1.8.0") "setuptools_scm"))
             #t)))))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (inputs
     `(("python-py" ,python-py)
       ("python-pytest" ,python-pytest)))
    (home-page "http://pytest-django.readthedocs.org/")
    (synopsis "A Django plugin for py.test")
    (description "Pytest-django is a plugin for py.test that provides a set of
useful tools for testing Django applications and projects.")
    (properties `((python2-variant . ,(delay python2-pytest-django))))
    (license license:bsd-3)))

(define-public python2-pytest-django
  (let ((base (package-with-python2
                (strip-python2-variant python-pytest-django))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-sqlalchemy-utils
  (package
    (name "python-sqlalchemy-utils")
    (version "0.32.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "cd/f3/f4399406309b9b37305bcadec371f417573aea16a2bf9990e0e8176a45ae"
               "/SQLAlchemy-Utils-" version ".tar.gz"))
        (sha256
         (base32
          "1rwmwvsym2m7x9mwdbp3l8axi9ancmscbbc4hdxja50iwbgcskgg"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)
       ("python-sqlalchemy" ,python-sqlalchemy)))
    (home-page "https://github.com/kvesteri/sqlalchemy-utils")
    (synopsis "Various utility functions for SQLAlchemy.")
    (description
     "Various utility functions for SQLAlchemy.")
    (properties `((python2-variant . ,(delay python2-sqlalchemy-utils))))
    (license license:bsd-3)))

(define-public python2-sqlalchemy-utils
  (let ((base (package-with-python2
                (strip-python2-variant python-sqlalchemy-utils))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-graphql-relay
  (package
    (name "python-graphql-relay")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "cf/dc/684ded66cab738f54907d3adc21ea8cc4bf395802c950e2709e36fc43a99"
               "/graphql-relay-" version ".tar.gz"))
            (sha256
             (base32
              "04wr9ayshxjjdcg2v21c7ffbz36kif1wjl3604fqd3qignb3fbxi"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (inputs
     `(("python-graphql-core" ,python-graphql-core)
       ("python-promise" ,python-promise)
       ("python-six" ,python-six)))
    (home-page "https://github.com/graphql-python/graphql-relay-py")
    (synopsis "Relay implementation for Python")
    (description "Relay implementation for Python")
    (properties `((python2-variant . ,(delay python2-graphql-relay))))
    (license license:expat)))

(define-public python2-graphql-relay
  (let ((base (package-with-python2
                (strip-python2-variant python-graphql-relay))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-graphql-core
  (package
    (name "python-graphql-core")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "53/74/7f71377be0d0fc9dbf47701853e6608b3da53ecd0e0c57f750d394455345"
               "/graphql-core-" version ".tar.gz"))
            (sha256
             (base32
              "0rsaarx2sj4xnw9966rhh4haiqaapm4lm2mfqm48ywd51j5vh1a0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; can't find gevent
    (native-inputs
     `(("python-gevent" ,python-gevent)
       ("python-mock" ,python-mock)
       ("python-pytest-mock" ,python-pytest-mock)))
    (inputs
     `(("python-promise" ,python-promise)
       ("python-six" ,python-six)))
    (home-page "https://github.com/graphql-python/graphql-core")
    (synopsis "GraphQL implementation for Python")
    (description "GraphQL implementation for Python")
    (properties `((python2-variant . ,(delay python2-graphql-core))))
    (license license:expat)))

(define-public python2-graphql-core
  (let ((base (package-with-python2
                (strip-python2-variant python-graphql-core))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base))))))

(define-public python-pytest-mock
  (package
    (name "python-pytest-mock")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
          (uri (string-append
                 "https://pypi.python.org/packages/"
                 "99/0e/45906c1e876b175cb51d8710075be900948f44a5f6a92c90095bdcd846c8"
                 "/pytest-mock-" version ".zip"))
        (sha256
         (base32
          "0gmlh1jzcs385d0764gshmmyszid70v8sc185pmz7gb97idza461"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("python-py" ,python-py)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/pytest-dev/pytest-mock/")
    (synopsis "Thin-wrapper around the mock package for easier use with py.test")
    (description
     "Thin-wrapper around the mock package for easier use with py.test")
    (properties `((python2-variant . ,(delay python2-pytest-mock))))
    (license license:expat)))

(define-public python2-pytest-mock
  (let ((base (package-with-python2
                (strip-python2-variant python-pytest-mock))))
    (package (inherit base)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs base)))
      (inputs
       `(("python2-mock" ,python2-mock)
         ,@(package-inputs base))))))

(define-public python-promise
  (package
    (name "python-promise")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "8b/e0/7da87005df50169833697637c1e8192f7a7ff72ffc39978833fde18c93da"
               "/promise-" version ".tar.gz"))
        (sha256
         (base32
          "1k19ms8l3d5jzjh557rgkxb5sg4mqgfc315rn4hx1z3n8qq6lr3h"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; tests wants python-futures, which doesn't exist for python3
    (home-page "https://github.com/syrusakbary/promise")
    (synopsis "Promises/A+ implementation for Python")
    (description
     "Promises/A+ implementation for Python")
    (properties `((python2-variant . ,(delay python2-promise))))
    (license license:expat)))

(define-public python2-promise
  (let ((promise (package-with-python2
                   (strip-python2-variant python-promise))))
    (package (inherit promise)
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ,@(package-native-inputs promise))))))

(define-public python-peewee
  (package
    (name "python-peewee")
    (version "2.8.1")
      (source
        (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "59/4a/a1b78b0e47e880c07da21d633ff2ac8d5edbf969049a414edfbdadaed869"
               "/peewee-" version ".tar.gz"))
        (sha256
         (base32
          "0zcqszn46ag1kbrhzl95lh9psrai0zj6mq13193jph4m9l991nwz"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; Import error when trying to run tests
    (inputs
     `(("python-cython" ,python-cython)))
    (home-page "http://github.com/coleifer/peewee/")
    (synopsis "a little orm")
    (description "a little orm")
    (license license:expat)))

(define-public python2-peewee
  (package-with-python2 python-peewee))

(define-public python-pika
  (package
    (name "python-pika")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "ee/25/1517ce612d7cd0a426ea027275ba74165bbfd86a2daf4bce4839afac3deb"
               "/pika-" version ".tar.gz"))
        (sha256
         (base32
          "0nb4h08di432lv7dy2v9kpwgk0w92f24sqc2hw2s9vwr5b8v8xvj"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-twisted" ,python-twisted)))
    (home-page "https://pika.readthedocs.org")
    (synopsis "Pika Python AMQP Client Library")
    (description
     "Pika is a pure-Python implementation of the AMQP 0-9-1 protocol that
tries to stay fairly independent of the underlying network support library.")
    (license license:bsd-3)))

(define-public python2-pika
  (package-with-python2 python-pika))
