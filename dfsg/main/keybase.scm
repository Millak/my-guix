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

(define-module (dfsg main keybase)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages syncthing))

;; TODO: Unbundle all the go dependencies. Remove electron GUI pieces.
(define-public keybase
  (package
    (name "keybase")
    (version "5.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/keybase/client/releases/download/v"
               version "/keybase-v" version ".tar.xz"))
        (sha256
         (base32
          "040jn7g3nq4qpf8kvizs40gc2cbdxsy6nkx4qpsc8agk032cgnlc"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (with-directory-excursion "go/vendor"
              (delete-file-recursively "github.com/blang")
              (delete-file-recursively "github.com/BurntSushi")
              (delete-file-recursively "github.com/gobwas")
              (delete-file-recursively "github.com/golang/groupcache/lru")
              (delete-file-recursively "github.com/golang/protobuf")
              (delete-file-recursively "github.com/golang/snappy")
              (delete-file-recursively "github.com/kr")
              (delete-file-recursively "github.com/lib")
              (delete-file-recursively "github.com/mattn")
              (delete-file-recursively "github.com/mitchellh")
              (delete-file-recursively "github.com/pkg/errors")
              (delete-file-recursively "github.com/pmezard")
              ;(delete-file-recursively "github.com/rcrowley")
              (delete-file-recursively "github.com/shirou")
              (delete-file-recursively "github.com/stathat")
              ;(delete-file-recursively "github.com/syndtr")
              (delete-file-recursively "github.com/urfave")
              (delete-file-recursively "github.com/willf")
              (delete-file-recursively "golang.org/x/crypto")
              (delete-file-recursively "golang.org/x/net")
              (delete-file-recursively "golang.org/x/sync/errgroup")
              (delete-file-recursively "golang.org/x/sys")
              (delete-file-recursively "golang.org/x/text")
              (delete-file-recursively "golang.org/x/time")
              )
            ;; Lets smallerize the source to audit less code and licenses.
            (delete-file-recursively "osx")
            (delete-file-recursively "shared/ios")
            ;; Remove non-free fonts.
            (with-directory-excursion "shared/fonts"
              (for-each (lambda (file)
                          (delete-file file))
                        (find-files "." "keybase.*ttf")))
            #t))))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:import-path "github.com/keybase/client/go/keybase"
       #:unpack-path "github.com/keybase/client"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 (invoke "go" "install"
                         "-tags" "production"
                         "-v" "-x" "-ldflags=-s -w"
                         directory))
               (list import-path
                     "github.com/keybase/client/go/kbfs/kbfsfuse"
                     "github.com/keybase/client/go/kbfs/kbfsgit/git-remote-keybase"
                     "github.com/keybase/client/go/kbfs/redirector"
                     "github.com/keybase/client/go/kbnm"))
             #t))
         (replace 'check
           (lambda* (#:key import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 (invoke "go" "test"
                         "-v" "-x" "-ldflags=-s -w"
                         directory))
               (list import-path
                     "github.com/keybase/client/go/kbfs/kbfsfuse"
                     "github.com/keybase/client/go/kbfs/kbfsgit/git-remote-keybase"
                     "github.com/keybase/client/go/kbfs/redirector"
                     "github.com/keybase/client/go/kbnm"))
             #t))
         (add-after 'install 'install-license
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "src/github.com/keybase/client/LICENSE"
                             (string-append out "/share/doc/"
                                            ,name "-" ,version "/"))
               #t))))))
    (inputs
     `(("go-github-com-blang-semver" ,go-github-com-blang-semver)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-gobwas-glob" ,go-github-com-gobwas-glob)
       ("go-github-com-golang-groupcache-lru" ,go-github-com-golang-groupcache-lru)
       ("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)
       ("go-github-com-golang-snappy" ,go-github-com-golang-snappy)
       ("go-github-com-kr-text" ,go-github-com-kr-text)
       ("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ;("go-github-com-rcrowley-go-metrics" ,go-github-com-rcrowley-go-metrics)
       ("go-github-com-shirou-gopsutil" ,go-github-com-shirou-gopsutil)
       ("go-github-com-stathat-go" ,go-github-com-stathat-go)
       ;("go-github-com-syndtr-goleveldb" ,go-github-com-syndtr-goleveldb)
       ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)
       ("go-github-com-willf-bitset" ,go-github-com-willf-bitset)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang.org-x-sync-errgroup" ,go-golang.org-x-sync-errgroup)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-time" ,go-golang-org-x-time)))
    (home-page "https://keybase.io")
    (synopsis "Secure messaging and file-sharing")
    (description "Keybase is a safe, secure, and private app for everything you
do online.")
    (license license:bsd-3)))
