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

(define-module (wip pipx)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  ;#:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-xyz)
  )

(define-public pipx
  (package
    (name "pipx")
    (version "0.15.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pipx" version))
        (sha256
         (base32
          "0bmpri17phgziqhasp332ygiaa6lbw6pf6ybii1fhk7a96akmah1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH" (string-append (getcwd) "/tests:" (getenv "PYTHONPATH")))
             (invoke "python3" "-m" "unittest" "tests.test_pipx.PipxStaticTests"))))))
    (propagated-inputs
     `(("python-argcomplete" ,python-argcomplete)
       ("python-userpath" ,python-userpath)))
    (home-page "https://github.com/pipxproject/pipx")
    (synopsis
     "Install and run Python applications in isolated environments")
    (description
     "pipx allows you to...
@itemize
@item Run the latest version of a CLI application from a package in a temporary
virtual environment, leaving your system untouched after it finishes.
@item Install packages to isolated virtual environments, while globally exposing
their CLI applications so you can run them from anywhere.
@item Easily list, upgrade, and uninstall packages that were installed with pipx.
@end itemize")
    (license (list license:bsd-3
                   license:expat))))

(define-public python-userpath
  (package
    (name "python-userpath")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "userpath" version))
        (sha256
         (base32
          "0s2ljqd40ifm8fj9jba2crjmj71jpr8rww3bbr662b3ka2k8n2yx"))))
    (build-system python-build-system)
    (arguments
     '(
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "TOXENV" "py37")
             (setenv "TOX_ENV_NAME" "testenv")
             (setenv "PATH" (string-append (assoc-ref outputs "out") "/bin:" (getcwd) ":" (getenv "PATH")))
             (setenv "PYTHONPATH" (string-append (getcwd) ":" (getenv "PYTHONPATH")))
             ;(invoke "python" "-m" "pytest" "-v")
             #t)))
       ))
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-distro" ,python-distro)))
    (native-inputs
     `(
       ("python-pytest" ,python-pytest)
       ))
    (home-page "https://github.com/ofek/userpath")
    (synopsis "Add locations to the user PATH")
    (description
     "@code{userpath} is a command-line tool and Python library to add custom
locations to user PATH.  Only user-specific PATH is changed, avoiding the need
for elevated privileges.")
    (license (list license:asl2.0
                   license:expat))))
