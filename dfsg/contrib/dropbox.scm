;;; Copyright © 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg contrib dropbox)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public dbxfs
  (package
    (name "dbxfs")
    (version "1.0.43")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dbxfs" version))
        (sha256
         (base32
          "1f9sy2ax215dxiwszrrcadffjdsmrlxm4kwrbiap9dhxvzm226ks"))
        (patches (search-patches "dbxfs-remove-sentry-sdk.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests requires safefs
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)
       ("python-block-tracing" ,python-block-tracing)
       ("python-dropbox" ,python-dropbox)
       ("python-keyring" ,python-keyring)
       ("python-keyrings.alt" ,python-keyrings.alt)
       ("python-privy" ,python-privy)
       ("python-userspacefs" ,python-userspacefs)))
  (home-page "https://github.com/rianhunter/dbxfs")
  (synopsis "User-space file system for Dropbox")
  (description
   "@code{dbxfs} allows you to mount your Dropbox folder as if it were a
local filesystem.")
  (license license:gpl3+)))

(define-public python-dropbox
  (package
    (name "python-dropbox")
    (version "9.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dropbox" version))
        (sha256
         (base32
          "0qid094qna6bl4zpd08f6snvipwjls1yadacvmwri11djgp0wvj3"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; Tests require a network connection.
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-certifi" ,python-certifi)
       ("python-chardet" ,python-chardet)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)
       ("python-urllib3" ,python-urllib3)))
    (home-page "https://www.dropbox.com/developers")
    (synopsis "Official Dropbox API Client")
    (description "A Python SDK for integrating with the Dropbox API v2.")
    (license license:expat)))
