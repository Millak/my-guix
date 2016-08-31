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

(define-module (wip onionshare)
  #:use-module (gnu packages)
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
          "0pc3xbq379415s0i0y6rz02hay20zbvgra1jmg4mgrl9vbdr8zmw"))
        (patches (search-patches "onionshare-fix-install-paths.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out        (assoc-ref outputs "out"))
                    (onionshare (string-append out "/share/onionshare")))
               (substitute*
                 "install/pyinstaller.spec"
                 ;; inform onionshare where the 'resources' files are installed
                 (("../resources") onionshare))
               (substitute*
                 "onionshare/strings.py"
                 ;; correct the locale directory
                 (("helpers.get_resource_path\\('locale'\\)")
                  (string-append "'" onionshare "/locale'")))
               (substitute*
                 "onionshare/helpers.py"
                 ;; correct the location of version.txt
                 (("/usr") out)
                 (("get_resource_path\\('version.txt'\\)")
                  (string-append "'" onionshare "/version.txt'"))
                 (("get_resource_path\\('wordlist.txt'\\)")
                  (string-append "'" onionshare "/wordlist.txt'")))
               (substitute*
                 "onionshare/web.py"
                 ;; fix the location of the html files
                 (("helpers.get_resource_path\\('html/denied.html'\\)")
                  (string-append "'" onionshare "/html/denied.html'"))
                 (("helpers.get_resource_path\\('html/404.html'\\)")
                  (string-append "'" onionshare "/html/404.html'"))
                 (("helpers.get_resource_path\\('html/index.html'\\)")
                  (string-append "'" onionshare "/html/index.html'")))
               (substitute*
                 "onionshare_gui/file_selection.py"
                 (("helpers.get_resource_path\\('images/drop_files.png'\\)")
                  (string-append "'" onionshare "/images/drop_files.png'")))
               (substitute*
                 "onionshare_gui/server_status.py"
                 (("helpers.get_resource_path\\('images/server_stopped.png'\\)")
                  (string-append "'" onionshare "/images/server_stopped.png'"))
                 (("helpers.get_resource_path\\('images/server_working.png'\\)")
                  (string-append "'" onionshare "/images/server_working.png'"))
                 (("helpers.get_resource_path\\('images/server_started.png'\\)")
                  (string-append "'" onionshare "/images/server_started.png'")))
               (substitute*
                 "onionshare_gui/onionshare_gui.py"
                 (("helpers.get_resource_path\\('images/logo.png'\\)")
                  (string-append "'" onionshare "/images/logo.png'")))
               (substitute*
                 "install/onionshare.desktop"
                 (("/usr") out))
             #t)))
         (delete 'check)
         (add-before 'strip 'tests
           ;; After all the patching we run the tests after installing.
           ;; This is also a known issue: https://github.com/micahflee/onionshare/issues/284
           (lambda _ (zero? (system* "nosetests" "test")))))
       ;; can't compress the egg because it expects to find all the resources
       ;; inside the egg as though it were a folder.
       #:configure-flags '("--single-version-externally-managed" "--root=/")
       ))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (inputs
     `(("python-flask" ,python-flask)
       ("python-nautilus" ,python-nautilus)
       ("python-sip" ,python-sip)
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
    (license (list
               license:gpl3+
               license:bsd-3)))) ; onionshare/socks.py

(define-public python-nautilus
  (package
    (name "python-nautilus")
    (version "0.4.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nautilus" version))
        (sha256
         (base32
          "01hwzjc1zshk4vvxrcghm398fpy4jls66dyz06g07mrwqif8878p"))))
  (build-system python-build-system)
  (arguments `(#:tests? #f)) ; fails to import test modules
  (native-inputs
   `(("python-graphql-core" ,python-graphql-core)
     ("python-graphql-relay" ,python-graphql-relay)
     ("python-pycparser" ,python-pycparser)
     ("python-requests" ,python-requests)
     ("python-setuptools" ,python-setuptools)))
  (inputs
   `(("python-bcrypt" ,python-bcrypt)
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
    (synopsis "Library for creating microservice applications")
    (description
     "Nautilus is a framework for flux based microservices that looks to
provide extendible implementations of common aspects of a cloud so that you can
focus on building massively scalable web applications.")
    (license license:expat)))

(define-public python-bcrypt
  (package
    (name "python-bcrypt")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "bcrypt" version))
        (sha256
         (base32
          "1giy0dvd8gvq6flxh44np1v2nqwsji5qsnrz038mgwzgp7c20j75"))))
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
     "Bcrypt is a Python module which provides a password hashing method based
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

(define-public python-nose2
(package
  (name "python-nose2")
  (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose2" version))
        (sha256
         (base32
          "1x4zjq1zlyrh8b9ba0cmafd3w94pxhid408kibyjd3s6h1lap6s7"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; TODO: re-enable
    (native-inputs
     `(("python-pytest-cov" ,python-pytest-cov)))
    (inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/nose-devs/nose2")
    (synopsis "Next generation of nicer testing for Python")
    (description
     "Nose2 is the next generation of nicer testing for Python, based on the
plugins branch of unittest2.  Nose2 aims to improve on nose by providing a
better plugin api, being easier for users to configure, and simplifying internal
interfaces and processes.")
    (license license:bsd-2)))

(define-public python2-nose2
  (package-with-python2 python-nose2))

(define-public python-consul
  (package
    (name "python-consul")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-consul" version))
        (sha256
         (base32
          "0rfyxcy4cr3x848vhx876ifalxd5ghq6l5x813m49h4vq2d4jiq8"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (home-page "https://github.com/cablehead/python-consul")
    (synopsis "Python client for Consul")
    (description
     "Python client for @url{http://www.consul.io/,Consul}, a tool for service
discovery, monitoring and configuration.")
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
        (uri (pypi-uri "graphene" version))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "django-filter" version))
        (sha256
         (base32
          "0f78hmk8c903zwfzlsiw7ivgag81ymmb5hi73rzxbhnlg2v0l3fx"))))
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
        (uri (pypi-uri "pytest-django" version))
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
    (synopsis "Django plugin for py.test")
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
    (version "0.32.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "SQLAlchemy-Utils" version))
        (sha256
         (base32
          "1zbmmh7n8m01ikizn2mj1mfwch26nsr1awv9mvskqry7av0mpy98"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)
       ("python-sqlalchemy" ,python-sqlalchemy)))
    (home-page "https://github.com/kvesteri/sqlalchemy-utils")
    (synopsis "Various utility functions for SQLAlchemy")
    (description
     "SQLAlchemy-utils provides various utility functions and custom data types
for SQLAlchemy.  SQLAlchemy is an SQL database abstraction library for Python.")
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
        (uri (pypi-uri "graphql-relay" version))
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
    (description
     "This is a library to allow the easy creation of Relay-compliant servers
using the GraphQL Python reference implementation of a GraphQL server.  It
should be noted that the code is a exact port of the original
@url{https://github.com/graphql/graphql-relay-js,graphql-relay js implementation}
from Facebook.")
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
        (uri (pypi-uri "graphql-core" version))
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
    (description
     "GraphQL implementation for Python.  GraphQL is a data query language and
runtime designed and used to request and deliver data to mobile and web apps.
This library is a port of @url{https://github.com/graphql/graphql-js,graphql-js}
to Python.")
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
    (version "1.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-mock" version ".zip"))
        (sha256
         (base32
          "03zxar5drzm7ksqyrwypjaza3cri6wqvpr6iam92djvg6znp32gp"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("python-py" ,python-py)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/pytest-dev/pytest-mock/")
    (synopsis "Thin-wrapper around the mock package for easier use with py.test")
    (description
     "This plugin installs a @code{mocker} fixture which is a thin-wrapper
around the patching API provided by the @code{mock} package, but with the
benefit of not having to worry about undoing patches at the end of a test.
The mocker fixture has the same API as @code{mock.patch}, supporting the
same arguments.")
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
        (uri (pypi-uri "promise" version))
        (sha256
         (base32
          "1k19ms8l3d5jzjh557rgkxb5sg4mqgfc315rn4hx1z3n8qq6lr3h"))))
    (build-system python-build-system)
    ;; Tests wants python-futures, which is a python2 only program, and
    ;; can't be found by python2-promise at test time.
    (arguments `(#:tests? #f))
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
    (version "2.8.3")
      (source
        (origin
        (method url-fetch)
        (uri (pypi-uri "peewee" version))
        (sha256
         (base32
          "1605bk11s7aap2q4qyba93rx7yfh8b11kk0cqi08z8klx2iar8yd"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Fails to import test data
    (native-inputs
     `(("python-cython" ,python-cython)))
    (home-page "https://github.com/coleifer/peewee/")
    (synopsis "Small object-relational mapping utility")
    (description
     "Peewee is a simple and small ORM (object-relation mapping) tool.  Peewee
handles converting between pythonic values and those used by databases, so you
can use Python types in your code without having to worry.  It has built-in
support for sqlite, mysql and postgresql.  If you already have a database, you
can autogenerate peewee models using @code{pwiz}, a model generator.")
    (license license:expat)))

(define-public python-pika
  (package
    (name "python-pika")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pika" version))
        (sha256
         (base32
          "0nb4h08di432lv7dy2v9kpwgk0w92f24sqc2hw2s9vwr5b8v8xvj"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-twisted" ,python-twisted)))
    (home-page "https://pika.readthedocs.org")
    (synopsis "Pure Python AMQP Client Library")
    (description
     "Pika is a pure-Python implementation of the AMQP (Advanced Message Queuing
Protocol) 0-9-1 protocol that tries to stay fairly independent of the underlying
network support library.")
    (license license:bsd-3)))

(define-public python2-pika
  (package-with-python2 python-pika))
