;;; Copyright © 2020, 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages syncthing)
  #:use-module (dfsg main golang))

(define-public keybase
  (package
    (name "keybase")
    (version "6.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/keybase/client")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0cky0slb3f53lqbdzls69j6xys6bqgiin07qfifvnvyyjzcc4r9j"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (for-each delete-file-recursively
                      (list "osx"
                            "shared"
                            "browser"       ; GUI
                            "protocol"      ; protocol generator and tester
                            "pvl-tools"
                            "media"
                            "packaging"))
            (substitute* (find-files "go" "\\.go$")
              (("github\\.com/stellar/go/build")
               "github.com/stellar/go/txnbuild")
              (("github.com/stellar/go/clients/horizon")
               "github.com/stellar/go/clients/horizonclient")
              )
            ))))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:import-path "github.com/keybase/client/go/keybase"
       #:unpack-path "github.com/keybase/client"
       #:build-flags '("-tags" "production")
       #:phases
       (modify-phases %standard-phases
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
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path directory))
               (list import-path
                     "github.com/keybase/client/go/kbfs/kbfsfuse"
                     "github.com/keybase/client/go/kbfs/kbfsgit/git-remote-keybase"
                     "github.com/keybase/client/go/kbfs/redirector"
                     "github.com/keybase/client/go/kbnm"))))
         (add-after 'install 'install-license
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "src/github.com/keybase/client/LICENSE"
                             (string-append out "/share/doc/"
                                            ,name "-" ,version "/"))))))))
    (inputs
     (list go-bazil-org-fuse
           go-camlistore-org-pkg-buildinfo ; camlistore/pkg/images
           go-camlistore-org-pkg-images
           go-github-com-antchfx-htmlquery
           go-github-com-antchfx-xmlquery
           go-github-com-araddon-dateparse
           go-github-com-blang-semver
           go-github-com-blevesearch-bleve
           go-github-com-btcsuite-btcutil
           go-github-com-buger-jsonparser
           go-github-com-coreos-go-systemd-v22
           go-github-com-deckarep-golang-set
           go-github-com-dustin-go-humanize-20150824
           go-github-com-eapache-channels
           go-github-com-gammazero-workerpool
           go-github-com-gobwas-glob
           go-github-com-gocolly-colly-debug
           go-github-com-gocolly-colly-storage
           go-github-com-go-errors-errors
           go-github-com-golang-groupcache
           go-github-com-golang-mock-gomock
           go-github-com-hashicorp-golang-lru
           go-github-com-kennygrant-sanitize
           ;go-github-com-keybase-backoff
           go-github-com-keybase-cli
           go-github-com-keybase-clockwork
           go-github-com-keybase-colly
           go-github-com-keybase-go-codec
           go-github-com-keybase-go-crypto
           go-github-com-keybase-go-framed-msgpack-rpc
           go-github-com-keybase-go-jsonw
           ;go-github-com-keybase-go-kext   ; macos specific
           go-github-com-keybase-go-keychain
           go-github-com-keybase-go-logging
           go-github-com-keybase-go-merkle-tree
           go-github-com-keybase-go-porterstemmer
           go-github-com-keybase-go-ps
           go-github-com-keybase-go-triplesec
           go-github-com-keybase-go-triplesec-insecure
           go-github-com-keybase-go-updater
           ;go-github-com-keybase-go-winio  ; win32 specific
           go-github-com-keybase-go-dbus
           go-github-com-keybase-golang-ico
           go-github-com-keybase-gomounts
           ;go-github-com-keybase-keybase-test-vectors
           go-github-com-keybase-pipeliner
           ;go-github-com-keybase-release
           go-github-com-keybase-saltpack
           go-github-com-keybase-stellarnet
           go-github-com-kr-text
           go-github-com-kyokomi-emoji
           go-github-com-mattn-go-isatty
           go-github-com-miekg-dns
           go-github-com-nfnt-resize
           go-github-com-pkg-errors
           go-github-com-puerkitobio-goquery
           go-github-com-qrtz-nativemessaging
           go-github-com-rcrowley-go-metrics
           go-github-com-shirou-gopsutil
           go-github-com-saintfish-chardet
           go-github-com-shopspring-decimal
           go-github-com-stellar-go-address
           ;go-github-com-stellar-go-build  ; Deprecated upstream, fails to unpack
           ;go-github-com-stellar-go-clients-federation ; fails to unpack
           ;go-github-com-stellar-go-clients-horizon  ; Deprecated upstream, fails to unpack
           go-github-com-stellar-go-clients-horizonclient
           go-github-com-stellar-go-clients-stellartoml
           go-github-com-stellar-go-crc16
           go-github-com-stellar-go-hash
           go-github-com-stellar-go-keypair
           go-github-com-stellar-go-network
           go-github-com-stellar-go-price
           go-github-com-stellar-go-protocols-federation
           go-github-com-stellar-go-protocols-horizon
           go-github-com-stellar-go-strkey
           ;go-github-com-stellar-go-support-clock  ; not in codebase
           go-github-com-stellar-go-support-errors
           ;go-github-com-stellar-go-support-http-httpdecode
           go-github-com-stellar-go-support-log
           go-github-com-stellar-go-support-render-hal
           go-github-com-stellar-go-support-render-httpjson
           go-github-com-stellar-go-support-render-problem
           go-github-com-stellar-go-support-url
           go-github-com-stellar-go-txnbuild
           go-github-com-stellar-go-xdr
           ;go-github-com-stellar-go
           go-github-com-stretchr-testify
           go-github-com-syndtr-goleveldb-leveldb  ; use keybase fork instead
           go-github-com-vividcortex-ewma
           go-github-com-temoto-robotstxt
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-text
           go-golang-org-x-time
           go-google-golang-org-appengine-internal
           go-google-golang-org-appengine-urlfetch
           go-gopkg-in-src-d-go-billy-v4
           go-gopkg-in-src-d-go-git-v4 ; use keybase fork instead
           go-mvdan-cc-xurls-v2
           go-rsc-io-qr
           go-stathat-com-c-ramcache))
    (home-page "https://keybase.io")
    (synopsis "Secure messaging and file-sharing")
    (description "Keybase is a safe, secure, and private app for everything you
do online.")
    (properties
      '((release-monitoring-url . "https://github.com/keybase/client/releases")))
    (license license:bsd-3)))

(define-public keybase-with-newer-go-libraries
  (package
    (inherit (newer-go-libraries keybase))
    (name "keybase-with-newer-go-libraries")))
