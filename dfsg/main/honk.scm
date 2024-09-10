;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main honk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages man)
  #:use-module (gnu packages sqlite))

;;;
;;;
;;;

(define-public honk
  (package
    (name "honk")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://humungus.tedunangst.com/"
                                  "r/honk/d/honk-" version ".tgz"))
              (sha256
               (base32 "18017qiib63l9f1kxxll53z002nc22p8rbr51risah5md5a15yli"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (for-each delete-file (find-files "docs" "\\.html$"))
                   (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
       #:install-source? #f
       #:import-path "humungus.tedunangst.com/r/honk"
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'set-default-viewdir
             (lambda* (#:key outputs import-path #:allow-other-keys)
               (with-directory-excursion
                 (string-append "src/" import-path)
                 (substitute* "main.go"
                   (("viewDir = \\\"\\.\\\"")
                    (string-append "viewDir = \"" #$output "/share/honk\""))))))
           (add-after 'install 'install-more
             (lambda* (#:key outputs import-path #:allow-other-keys)
               (let ((man (string-append #$output "/share/man")))
                 (with-directory-excursion
                   (string-append "src/" import-path)
                   (for-each (lambda (file)
                               (install-file file
                                             (string-append
                                               man "/man"
                                               (string-take-right file 1))))
                             (find-files "docs" "\\.[[:digit:]]$"))
                   (copy-recursively "views"
                                     (string-append #$output
                                                    "/share/honk/views")))))))))
    (inputs
     (list sqlite
           go-github-com-andybalholm-cascadia       ; 1.3.1
           go-github-com-gorilla-mux                ; 1.8.0
           go-github-com-mattn-go-runewidth         ; 0.0.13
           go-golang-org-x-crypto                   ; 0.12.0
           go-golang-org-x-net                      ; 0.14.0
           go-humungus-tedunangst-com-r-go-sqlite3  ; 1.1.3
           go-humungus-tedunangst-com-r-webs))      ; 0.7.9
    (native-inputs
     (list mandoc))
    (home-page "https://humungus.tedunangst.com/r/honk")
    (synopsis "Minimal ActivityPub server")
    (description
     "Take control of your honks and join the federation.  An ActivityPub server
with minimal setup and support costs.  Spend more time using the software and
less time operating it.")
    (license license:isc)))

(define-public go-humungus-tedunangst-com-r-go-sqlite3
  (package
    (name "go-humungus-tedunangst-com-r-go-sqlite3")
    (version "1.1.3")
    (source
     (origin
       (method hg-fetch)
       (uri (hg-reference (url "https://humungus.tedunangst.com/r/go-sqlite3")
                          (changeset (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "1xkx0ijljricbqyf98dgqcc2lx65a1h19ab8rx7vrimhyp7dw5c6"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "humungus.tedunangst.com/r/go-sqlite3"))
    (inputs
     (list sqlite))
    (home-page "https://humungus.tedunangst.com/r/go-sqlite3")
    (synopsis "go-sqlite3")
    (description "Package sqlite3 provides interface to SQLite3 databases.")
    (license license:expat)))

(define-public go-humungus-tedunangst-com-r-webs
  (package
    (name "go-humungus-tedunangst-com-r-webs")
    (version "0.7.9")
    (source
     (origin
       (method hg-fetch)
       (uri (hg-reference (url "https://humungus.tedunangst.com/r/webs")
                          (changeset (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "1xhmb7d3p201ps4bfcy5czgjzlv8ngnqf7aismcpvgik01ff36kl"))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "humungus.tedunangst.com/r/webs"
       #:unpack-path "humungus.tedunangst.com/r/webs"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path build-flags #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'build)
                    #:build-flags build-flags
                    #:import-path
                    (string-append "humungus.tedunangst.com/r/webs/" directory)))
                 (list "cache"
                       "gate"
                       ;"gencache"      ; This one fails with gccgo
                       "htfilter"
                       "httpsig"
                       "image"
                       "junk"
                       "log"
                       "login"
                       "mz"
                       "rss"
                       "synlight"
                       "templates"))))
           (replace 'check
             (lambda* (#:key tests? import-path #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'check)
                    #:tests? tests?
                    #:import-path
                    (string-append "humungus.tedunangst.com/r/webs/" directory)))
                 (list "cache"
                       "gate"
                       ;"gencache"      ; This one fails with gccgo
                       "htfilter"
                       "httpsig"
                       "image"
                       "junk"
                       "log"
                       "login"
                       "mz"
                       "rss"
                       "synlight"
                       "templates")))))))
    (propagated-inputs
     (list go-golang-org-x-net          ; 0.14.0
           go-golang-org-x-image        ; 0.11.0
           go-golang-org-x-crypto))     ; 0.12.0
    (home-page "https://humungus.tedunangst.com/r/webs")
    (synopsis "Web utilities")
    (description "This package contains a collection of web utilities.")
    (license license:isc)))
