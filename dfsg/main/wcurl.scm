;;; Copyright © 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main wcurl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages guile))

(define-public wcurl
  (package
    (name "wcurl")
    (version "2026.01.05")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/curl/wcurl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1br5rl50h8cy1w0r9pfclsn8ykaxryg8nk47kpmbp2walq6wyv7d"))))
    (build-system copy-build-system)
    (arguments
     (list
       #:modules '((guix build copy-build-system)
                   (guix build utils)
                   (srfi srfi-26))
       #:install-plan
       #~`(("wcurl" "bin/")
           ("wcurl.1" "share/man/man1/"))
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'install 'wrap-script
             (lambda* (#:key inputs #:allow-other-keys)
               (wrap-script "wcurl"
                 `("PATH" ":" prefix
                   ,(map dirname
                         (map (cut search-input-file inputs <>)
                              (list "bin/curl"
                                    "bin/sed")))))))
           (add-after 'wrap-script 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "./tests/tests.sh")))))))
    (inputs (list curl guile-3.0 sed))
    (native-inputs (list shunit2))
    (home-page "https://curl.se/wcurl/")
    (synopsis "Wrapper around curl to easily download files")
    (description "@command{wcurl} is a command line tool which lets you
download URLs without having to remember any parameters.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See COPYING in the distribution."))))
