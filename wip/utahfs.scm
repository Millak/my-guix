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
          ;(modules '((guix build utils)))
          ;(snippet
          ; '(begin
          ;    (delete-file-recursively "vendor")
          ;    #t))
          ))
      (build-system go-build-system)
      (arguments
       `(#:install-source? #f
         #:import-path "github.com/cloudflare/utahfs"
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   (invoke "go" "install"
                           "-v" "-x" "-ldflags=-s -w"
                           directory))
                 (list (string-append import-path "/cmd/utahfs-client")
                       (string-append import-path "/cmd/utahfs-server")))
               #t))
           (add-after 'install 'install-license
             (lambda* (#:key import-path outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file (string-append "src/" import-path "/LICENSE")
                               (string-append out "/share/doc/"
                                              ,name "-" ,version "/"))
                 #t))))))
      (inputs
       `(;("go-github-com-aws-aws-sdk-go" ,go-github-com-aws-aws-sdk-go)
         ;("go-github-com-jacobsa-fuse" ,go-github-com-jacobsa-fuse)
         ;("go-github-com-mattn-go-sqlite3" ,go-github-com-mattn-go-sqlite3)
         ;("go-github-com-prometheus-client_golang" ,go-github-com-prometheus-client_golang)
         ("go-github-com-prometheus-client-golang" ,go-github-com-prometheus-client-golang)
         ;("go-gopkg-in-kothar-go-backblaze-v0" ,go-gopkg-in-kothar-go-backblaze-v0)
         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
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
