;;; Copyright © 2020, 2021, 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 umanwizard <brennan@umanwizard.com>
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

(define-module (dfsg contrib keybase)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages nss)
  #:use-module (ice-9 match))

(define-record-type* <go-git-reference>
  go-git-reference make-go-git-reference
  go-git-reference?
  (url    go-git-reference-url)
  (commit go-git-reference-commit)
  (hash   go-git-reference-sha256))

(define-record-type* <go-url-reference>
  go-url-reference make-go-url-reference
  go-url-reference?
  (url  go-url-reference-url)
  (hash go-url-reference-hash))

(define* (go-fetch-vendored uri hash-algorithm hash-value name #:key system)
  (let ((src
          (match uri
                 (($ <go-git-reference> url commit hash)
                  (origin
                    (method git-fetch)
                    (uri (git-reference
                           (url url)
                           (commit commit)))
                    (sha256 hash)))
                 (($ <go-url-reference> url commit hash)
                  (origin
                    (method url-fetch)
                    (uri url)
                    (sha256 hash)))))
        (name (or name "go-git-checkout")))
    (gexp->derivation
      (string-append name "-vendored.tar.gz")
      (with-imported-modules (append '((guix build utils))
                                     %default-gnu-imported-modules)
        #~(begin
            (use-modules ((guix build gnu-build-system) #:prefix gnu:)
                         (guix build utils))
            (let ((inputs (list
                            #+go-1.23
                            #+tar
                            #+bzip2
                            #+gzip)))
              (set-path-environment-variable "PATH" '("/bin") inputs))
            ((assoc-ref gnu:%standard-phases 'unpack) #:source #$src)

            (setenv "GOCACHE" "/tmp/gc")
            (setenv "GOMODCACHE" "/tmp/gmc")
            (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs/"))

            (with-directory-excursion "go"
              (invoke "go" "mod" "vendor"))

            (invoke "tar" "czvf" #$output
                    ;; Avoid non-determinism in the archive.
                    "--mtime=@0"
                    "--owner=root:0"
                    "--group=root:0"
                    "--sort=name"
                    "--hard-dereference"
                    "../")))
      #:hash hash-value
      #:hash-algo hash-algorithm)))

(define-public keybase
  (package
    (name "keybase")
    (version "6.5.1")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/keybase/client")
                    (commit (string-append "v" version))
                    (hash
                     (base32
                      "0f33i9c9bdjp4y412glksqb7ncgcgaj30r2scm1q2cshrivdwyq7"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pvczym7v4whdfcdv86rqxpj9pa0cm7fg7xqf53r3ap1c17zd6zv"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (for-each delete-file-recursively
                             (list "osx"
                                   "shared"
                                   "browser"    ; GUI
                                   "protocol"   ; protocol generator and tester
                                   "pvl-tools"
                                   "media"
                                   "packaging"))))))
    (build-system go-build-system)
    (arguments
     (list
       #:go go-1.23
       #:install-source? #f
       #:import-path "github.com/keybase/client/go/keybase"
       #:unpack-path "github.com/keybase/client"
       #:build-flags #~(list "-tags" "production")
       #:phases
       #~(modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path build-flags #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'build)
                    #:build-flags build-flags
                    #:import-path directory))
                 (list import-path
                       "github.com/keybase/client/go/kbfs/kbfsfuse"
                       "github.com/keybase/client/go/kbfs/kbfsgit/git-remote-keybase"
                       "github.com/keybase/client/go/kbfs/redirector"
                       "github.com/keybase/client/go/kbnm"))))
           (replace 'check
             (lambda* (#:key import-path tests? #:allow-other-keys)
               (when tests?
                 (with-directory-excursion (string-append "src/" import-path)
                   (invoke "go" "test" "-v" "go/...")))))
           (add-after 'install 'install-license
             (lambda _
               (install-file "src/github.com/keybase/client/LICENSE"
                             (string-append #$output "/share/doc/"
                                            #$name "-" #$version "/"))))
           (add-after 'install 'install-shell-completions
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out #$output)
                      (keybase (string-append out "/bin/keybase"))
                      (bash (string-append out "/share/bash-completion/completions/keybase")))
                 (mkdir-p (dirname bash))
                 (with-output-to-file bash
                   (lambda ()
                     (invoke keybase "--generate-bash-completion")))))))))
    (home-page "https://keybase.io")
    (synopsis "Secure messaging and file-sharing")
    (description "Keybase is a key directory that maps social media identities
to encryption keys (including, but not limited to PGP keys) in a publicly
auditable manner.  Additionally it offers an end-to-end encrypted chat and
cloud storage system, called Keybase Chat and the Keybase Filesystem
respectively.  Files placed in the public portion of the filesystem are served
from a public endpoint, as well as locally from a filesystem union-mounted by
the Keybase client.")
    ;; Release-monitoring-url doesn't work with go-git-reference.
    ;(properties
    ; '((release-monitoring-url . "https://github.com/keybase/client/releases")))
    (license license:bsd-3)))
