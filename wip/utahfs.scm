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

(define-module (wip utahfs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages syncthing))

;; There are bug fixes since the last release.
(define-public utahfs
  (let ((commit "278bc1dbcd50cccd24754a228986ba506dde5649")
        (revision "1"))
    (package
      (name "utahfs")
      (version (git-version "1.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/cloudflare/utahfs")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0cnp146crc3c6ngdldbzhph3rc8x71zxpcy1p4ann1r6vpkgp5db"))
          (modules '((guix build utils)))
          (snippet
           '(begin
              ;; Check the Gopkg.toml
              ;(delete-file-recursively "vendor")
              ;(delete-file-recursively "vendor/cloud.google.com/go/storage") v1.10.0
              (delete-file-recursively "vendor/github.com/aws/aws-sdk-go")  ;v1.34.32
              (delete-file-recursively "vendor/github.com/jacobsa/fuse")
              (delete-file-recursively "vendor/github.com/mattn/go-sqlite3")    ; remove?
              (delete-file-recursively "vendor/github.com/prometheus/client_golang")    ; v1.7.1
              (delete-file-recursively "vendor/golang.org/x/crypto")
              (delete-file-recursively "vendor/gopkg.in/kothar/go-backblaze.v0")
              (delete-file-recursively "vendor/gopkg.in/yaml.v2")
              ;(delete-file-recursively "vendor/modernc.org/sqlite")    ; v1.7.3
              #t))))
      (build-system go-build-system)
      (arguments
       `(#:install-source? #f
         #:import-path "github.com/cloudflare/utahfs"
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path build-flags #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'build)
                    #:build-flags build-flags
                    #:import-path directory))
                 (list (string-append import-path "/cmd/utahfs-client")
                       (string-append import-path "/cmd/utahfs-server")))
               #t)))))
      (inputs
       `(("go-github-com-aws-aws-sdk-go" ,go-github-com-aws-aws-sdk-go)
         ("go-github-com-jacobsa-fuse" ,go-github-com-jacobsa-fuse)
         ("go-github-com-mattn-go-sqlite3" ,go-github-com-mattn-go-sqlite3)
         ("go-github-com-prometheus-client-golang" ,go-github-com-prometheus-client-golang)
         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
         ("go-gopkg-in-kothar-go-backblaze-v0" ,go-gopkg-in-kothar-go-backblaze-v0)
         ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
      (home-page "https://github.com/cloudflare/utahfs")
      (synopsis "Encrypted storage system backed by cloud storage")
      (description "UtahFS is an encrypted storage solution.  It has a FUSE
binding that creates a synthetic drive on the user's computer that they can
interact with like an external hard-drive.  Files stored in the drive are
uploaded to a cloud storage provider, which means the drive will never run out
of space and minimizes the likelihood of any files being lost.  The files are
encrypted such that the cloud storage provider knows almost nothing about
what's being stored.")
      (license license:bsd-3))))

(define-public go-github-com-jacobsa-fuse
  (let ((commit "05606cde59ac2f1595bdd9bfc44385182e68bb45")
        (revision "1"))
    (package
      (name "go-github-com-jacobsa-fuse")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/jacobsa/fuse")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0i23vsw3iky2vwzbcdp2aiwrzjm1c5w6y06brips1fq5cbdpszn6"))))
      (build-system go-build-system)
      (arguments
       `(#:tests? #f    ; tests try to use fusermount
         #:import-path "github.com/jacobsa/fuse"))
      (inputs
       `(("fuse" ,fuse-3)))
      (propagated-inputs
       `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
      (home-page "https://github.com/jacobsa/fuse")
      (synopsis "Go package for implementing a FUSE file system")
      (description "This package allows for writing and mounting user-space
file systems from Go.")
      (license license:asl2.0))))

(define-public go-gopkg-in-kothar-go-backblaze-v0
  (let ((commit "7594ed38700f5ca3fc7da8f8c7b91b0526ee66f2")
        (revision "1"))
    (package
      (name "go-gopkg-in-kothar-go-backblaze-v0")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/kothar/go-backblaze.v0")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0zgd5jwwdrqrnx828yxkzywh1dnwqi9y8pmq0vsndi6bwdp6dvkb"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "gopkg.in/kothar/go-backblaze.v0"))
      (propagated-inputs
       `(("go-github-com-google-readahead" ,go-github-com-google-readahead)
         ("go-github-com-pquerna-ffjson" ,go-github-com-pquerna-ffjson)))
      (home-page "https://gopkg.in/kothar/go-backblaze.v0")
      (synopsis "Golang client for Backblaze's B2 storage")
      (description
       "This package provides a Golang client for Backblaze's B2 storage.")
      (license license:expat))))

(define-public go-github-com-google-readahead
  (let ((commit "eaceba16903255cb149d1efc316f6cc83d765268")
        (revision "1"))
    (package
      (name "go-github-com-google-readahead")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/google/readahead")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "19l6a29yyypgpqf7v5877ni0bqgxfp41q7ffp2xj57rvikimwiyb"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/google/readahead"))
      (propagated-inputs
       `(("go-github-com-golang-glog" ,go-github-com-golang-glog)))
      (home-page "https://github.com/google/readahead")
      (synopsis "Enable concurrent reads from seekable or compressed files")
      (description "Readahead is a package that provides readers that enable
concurrent reads from seekable or compressed files.  It's useful when reading
from a network file system.")
      (license license:asl2.0))))

(define-public go-github-com-golang-glog
  (let ((commit "23def4e6c14b4da8ac2ed8007337bc5eb5007998")
        (revision "1"))
    (package
      (name "go-github-com-golang-glog")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/golang/glog")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0jb2834rw5sykfr937fxi8hxi2zy80sj2bdn9b3jb4b26ksqng30"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/golang/glog"))
      (home-page "https://github.com/golang/glog")
      (synopsis "Leveled execution logs for Go")
      (description "This is an efficient pure Go implementation of leveled logs
in the manner of the open source C++ package")
      (license license:asl2.0))))

(define-public go-github-com-pquerna-ffjson
  (let ((commit "aa0246cd15f76c96de6b96f22a305bdfb2d1ec02")
        (revision "1"))
    (package
      (name "go-github-com-pquerna-ffjson")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/pquerna/ffjson")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0mxmrvqmiinqhlaxncaqznxwfspf3p8bmg9vniz40dw5jpv24cwb"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/pquerna/ffjson"))
      (home-page "https://github.com/pquerna/ffjson")
      (synopsis "Faster JSON serialization for Go")
      (description "@code{ffjson} generates static @code{MarshalJSON} and
@code{UnmarshalJSON} functions for structures in Go.  The generated functions
reduce the reliance upon runtime reflection to do serialization and are
generally 2 to 3 times faster.  In cases where ffjson doesn't understand a Type
involved, it falls back to encoding/json, meaning it is a safe drop in
replacement.  By using ffjson your JSON serialization just gets faster with no
additional code changes.")
      (license license:asl2.0))))
