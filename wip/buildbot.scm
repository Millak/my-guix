;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
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
;;; You should have received a trivial of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (wip buildbot)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  )

;; Keep all the packages at the same version.
(define %buildbot-version "3.5.0")

(define-public buildbot
  (package
    (name "buildbot")
    (version %buildbot-version)
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "buildbot" version))
              (sha256
               (base32
                "0d1041bsis3576zg5b0caj0l8vgkk92kcs2bsgls9rm74rs8g0f2"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-source
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* '("buildbot/steps/master.py"
                                  "buildbot/util/git.py")
                     (("/bin/sh") (search-input-file inputs "/bin/sh")))))
               (add-before 'check 'skip-some-tests
                 (lambda _
                   ;(substitute* "buildbot/test/integration/interop/test_commandmixin.py"
                   ;  (("CommandMixinMasterMsgPack" all) (string-append "skip_" all)))
                   ;(substitute* "buildbot/test/integration/interop/test_compositestepmixin.py"
                   ;  (("CompositeStepMixinMasterMsgPack" all) (string-append "skip_" all)))
                   ;(substitute* "buildbot/test/integration/interop/test_integration_secrets.py"
                   ;  (("SecretsConfigMsgPack" all) (string-append "skip_" all)))

                   ;(substitute* "buildbot/test/integration/interop/test_setpropertyfromcommand.py"
                   ;  (("test_setProp" all) (string-append "_" all)))
                   ;(substitute* "buildbot/test/integration/interop/test_worker_reconnect.py"
                   ;  (("test_eventually_reconnect" all) (string-append "_" all)))
                   ;; Guix isn't recognized in the build container.
                   (substitute* "buildbot/test/unit/test_buildbot_net_usage_data.py"
                     (("test_linux_distro" all) (string-append "_" all)))
                   ))
               (add-after 'unpack 'no-windows-service
                 (lambda _
                   (substitute* "setup.py"
                     ((".*windows_service.*") "")))))))
    (propagated-inputs
     (list python-alembic
           python-autobahn
           python-dateutil
           python-jinja2
           python-msgpack
           python-pyjwt
           python-sqlalchemy
           python-twisted
           python-txaio
           python-pyyaml
           python-zope-interface))
    (native-inputs
     (list
       buildbot-worker
       python-boto3
       python-lz4
           python-mock
           python-moto
           python-parameterized
           python-pypugjs
           python-setuptools-trial
           python-treq
           python-txrequests
           python-wheel))
    (home-page "http://buildbot.net/")
    (synopsis "Continuous Integration Framework")
    (description "The Continuous Integration Framework")
    (license license:gpl2)))

(define buildbot-minimal
  (package
    (inherit buildbot)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'no-windows-service
                 (lambda _
                   (substitute* "setup.py"
                     ((".*windows_service.*") "")))))))
    (native-inputs '())))

(define-public buildbot-worker
  (package
    (name "buildbot-worker")
    (version %buildbot-version)
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "buildbot-worker" version))
              (sha256
               (base32
                "1h6qzz3c3iyhly0cj645p0swy0sany9xpbx7gzvilxprs56zg48x"))))
    (build-system python-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "buildbot_worker/runprocess.py"
                 (("/bin/sh") (search-input-file inputs "/bin/sh")))))
           (add-after 'unpack 'no-windows-service
             (lambda _
               (delete-file "buildbot_worker/scripts/windows_service.py")
               (substitute* "setup.py"
                 ((".*windows_service.*") ""))))
           (add-before 'check 'pre-check
             (lambda _
               ;; No PTYs in the build environment.
               (substitute* "buildbot_worker/test/unit/test_runprocess.py"
                 (("test_.*_usePTY" all) (string-append "_" all))))))))
    (propagated-inputs
     (list python-autobahn
           python-future
           python-msgpack
           python-twisted))
    (native-inputs
     (list python-flake8
           python-mock
           python-pep8
           python-parameterized
           python-psutil
           python-pyenchant
           python-pylint
           python-setuptools-trial
           python-wheel))
    (home-page "http://buildbot.net/")
    (synopsis "Buildbot Worker Daemon")
    (description "This package contains the Buildbot Worker Daemon.")
    (license license:gpl2)))

(define-public buildbot-www
  (package
    (name "buildbot-www")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "buildbot-www" version))
              (sha256
               (base32
                "1yv2zyvpfngdl5bff8zr6kx11skynjlggrvzad6hh75zd0qvz1kf"))))
    (build-system python-build-system)
    (propagated-inputs
     (list buildbot))
    (native-inputs
     (list buildbot-pkg))
    (home-page "http://buildbot.net/")
    (synopsis "Buildbot UI")
    (description "This package contains the buildbot UI.")
    (license license:gpl2)))

(define-public buildbot-badges
  (package
    (name "buildbot-badges")
    (version %buildbot-version)
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "buildbot-badges" version))
              (sha256
               (base32
                "0ll5kyb04j8ik1rgldxr9gwyirlwvdb0lqxgwqynhkbfil5nyxrm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cairocffi
           python-cairosvg
           python-jinja2
           python-klein))
    (native-inputs
     (list buildbot-minimal buildbot-pkg))
    (home-page "http://buildbot.net/")
    (synopsis "Buildbot badges")
    (description "This package contains the buildbot badges.")
    (license license:gpl2)))

(define-public buildbot-console-view
  (package
    (name "buildbot-console-view")
    (version %buildbot-version)
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "buildbot-console-view" version))
              (sha256
               (base32
                "0ks94iy2c7k80qfi5ba2sln7svr2vxdlwkcwjh7apf8h171d6h1m"))))
    (build-system python-build-system)
    (native-inputs
     (list buildbot-minimal buildbot-pkg))
    (home-page "http://buildbot.net/")
    (synopsis "Buildbot Console View plugin")
    (description "This package contains the buildbot Console View plugin.")
    (license license:gpl2)))

(define-public buildbot-pkg
  (package
    (name "buildbot-pkg")
    (version %buildbot-version)
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "buildbot-pkg" version))
              (sha256
               (base32
                "18wy0nbb9ljyi1bcj62imy6xyzz2p4sys2axlmr5qryr2y5wr1h9"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))    ; No tests included in tarball.
    (home-page "http://buildbot.net/")
    (synopsis "Buildbot packaging tools")
    (description "This package contains the buildbot packaging tools.")
    (license license:gpl2)))

(define-public python-klein
  (package
    (name "python-klein")
    (version "21.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "klein" version))
              (sha256
               (base32
                "1mpydmz90d0n9dwa7mr6pgj5v0kczfs05ykssrasdq368dssw7ch"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-attrs
           python-hyperlink
           python-incremental
           python-tubes
           python-twisted
           python-werkzeug
           python-zope-interface))
    (home-page "https://github.com/twisted/klein")
    (synopsis "werkzeug + twisted.web")
    (description "werkzeug + twisted.web")
    (license license:expat)))

(define-public python-tubes
  (package
    (name "python-tubes")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Tubes" version))
              (sha256
               (base32
                "12ypy2cibi6nyss17in9fnxjkixdjxbya798qdmzrxx65xzikfar"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))        ; TODO: Fix
    (propagated-inputs
     (list python-six
           python-twisted))
    (home-page "https://github.com/twisted/tubes/")
    (synopsis "Flow control and backpressure for event-driven applications.")
    (description
     "Flow control and backpressure for event-driven applications.")
    (license license:expat)))

(define-public python-setuptools-trial
  (package
    (name "python-setuptools-trial")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_trial" version))
              (sha256
               (base32
                "04lvyl7jyhi0w8n3i2q9szx59kvpa0cqgw164lgblj0wfs7hy8hl"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))        ; Skip the tests
    (propagated-inputs
     (list python-twisted))
    (home-page "https://github.com/rutsky/setuptools-trial")
    (synopsis
     "Setuptools plugin that makes unit tests execute with trial instead of pyunit.")
    (description
     "Setuptools plugin that makes unit tests execute with trial instead of pyunit.")
    (license license:bsd-3)))

(define-public python-pypugjs
  (package
    (name "python-pypugjs")
    (version "5.9.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pypugjs" version))
              (sha256
               (base32
                "0h8gknh0r61p7zxxg6lcacjvga0qrgw97i90z92z4w5wai7mlawi"))))
    (build-system python-build-system)
    (arguments
     (list
       #:tests? #f      ; Tests not included
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "make" "init")
                 (invoke "make" "test")))))))
    (propagated-inputs
     (list python-chardet
           python-six))
    (native-inputs
     (list python-django
           python-jinja2
           python-mako
           python-nose
           python-pyramid
           python-pyramid-mako
           python-tornado))
    (home-page "https://github.com/kakulukia/pypugjs")
    (synopsis
     "PugJS syntax template adapter for Django, Jinja2, Mako and Tornado templates")
    (description
     "PugJS syntax template adapter for Django, Jinja2, Mako and Tornado templates")
    (license license:expat)))

(define-public python-pyramid-mako
  (package
    (name "python-pyramid-mako")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyramid_mako" version))
              (sha256
               (base32
                "1qj0m091mnii86j2q1d82yir22nha361rvhclvg3s70z8iiwhrh0"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-mako python-pyramid))
    (native-inputs (list python-coverage python-nose python-webtest))
    (home-page "https://github.com/Pylons/pyramid_mako")
    (synopsis "Mako template bindings for the Pyramid web framework")
    (description "Mako template bindings for the Pyramid web framework")
    (license license:repoze)))

(define-public python-txrequests
  (package
    (name "python-txrequests")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "txrequests" version))
              (sha256
               (base32
                "1qb22fgyi66a84c2swxlsl3lksqg6cm94izsyrw5c02dzb5a2lml"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))        ; Tests require network connection.
    (propagated-inputs
     (list python-requests python-twisted))
    (home-page "https://github.com/tardyp/txrequests")
    (synopsis "Asynchronous Python HTTP for Humans")
    (description "Asynchronous Python HTTP for Humans.")
    (license license:asl2.0)))
