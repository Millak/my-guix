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

(define-module (wip gotosocial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages syncthing)
  #:use-module (dfsg main golang))

;;;
;;;
;;;

(define-public gotosocial
  (package
    (name "gotosocial")
    (version "0.7.1")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/superseriousbusiness"
                                  "/gotosocial/releases/download/v" version
                                  "/gotosocial-" version "-source-code.tar.gz"))
              (sha256
               (base32 "0x2impm9vjqvpv3p9kvpp1rdyhsi95hq0pwcnf71nilymbq0qjyf"))
              ;(snippet
              ; #~(begin
              ;     (use-modules (guix build utils))
              ;     (delete-file-recursively "vendor")
              ;     ))
              ))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:go ,go-1.19
       #:unpack-path "github.com/superseriousbusiness/gotosocial"
       #:import-path "github.com/superseriousbusiness/gotosocial/cmd/gotosocial"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? unpack-path #:allow-other-keys)
             (when tests?
               (with-directory-excursion (string-append "src/" unpack-path)
                 (setenv "GTS_DB_TYPE" "sqlite")
                 (setenv "GTS_DB_ADDRESS" ":memory:")
                 (invoke "go" "test" "-tags"
                         "'netgo osusergo static_build kvformat'"
                         "-count" "1" "./...")
                 (invoke "sh" "./test/envparsing.sh")))))
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (string-append
                            out "/etc/bash_completion.d/gotosocial"))
                    (fish (string-append
                            out "/share/vendor_completions.d/gotosocial.fish"))
                    (zsh  (string-append
                            out "/share/zsh/site-functions/_gotosocial")))
               (mkdir-p (dirname bash))
               (mkdir-p (dirname fish))
               (mkdir-p (dirname zsh))
               (with-output-to-file bash
                 (lambda _
                   (invoke (string-append out "/bin/gotosocial")
                           "completion" "bash")))
               (with-output-to-file fish
                 (lambda _
                   (invoke (string-append out "/bin/gotosocial")
                           "completion" "fish")))
               (with-output-to-file zsh
                 (lambda _
                   (invoke (string-append out "/bin/gotosocial")
                           "completion" "zsh"))))))
         (add-after 'install 'install-assets
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (web (assoc-ref inputs "gotosocial-web-assets.tar.gz")))
               (mkdir-p (string-append out "/share/gotosocial"))
               (with-directory-excursion (string-append out "/share/gotosocial")
                 (invoke "tar" "xvf" web))))))))
    (inputs
     (list
       sqlite
       ;codeberg.org/gruf/go-bytesize v1.0.2
       ;codeberg.org/gruf/go-byteutil v1.1.2
       ;codeberg.org/gruf/go-cache/v3 v3.2.3
       ;codeberg.org/gruf/go-debug v1.3.0
       ;codeberg.org/gruf/go-errors/v2 v2.1.1
       ;codeberg.org/gruf/go-kv v1.6.0
       ;codeberg.org/gruf/go-logger/v2 v2.2.1
       ;codeberg.org/gruf/go-mutexes v1.1.5
       ;codeberg.org/gruf/go-runners v1.6.0
       ;codeberg.org/gruf/go-store/v2 v2.2.1
       ;github.com/buckket/go-blurhash v1.1.0
       go-github-com-coreos-go-oidc             ; v3.5.0
       ;github.com/cornelk/hashmap v1.0.8
       go-github-com-disintegration-imaging     ; v1.6.2
       ;github.com/gin-contrib/cors v1.4.0
       ;github.com/gin-contrib/gzip v0.0.6
       ;github.com/gin-contrib/sessions v0.0.5
       ;github.com/gin-gonic/gin v1.8.2
       go-github-com-go-fed-httpsig             ; v1.1.0
       go-github-com-go-playground-validator-v10    ; v10.11.2
       go-github-com-google-uuid                ; v1.3.0
       go-github-com-gorilla-feeds              ; v1.1.1
       go-github-com-gorilla-websocket          ; v1.5.0
       ;github.com/h2non/filetype v1.1.3
       go-github-com-jackc-pgconn               ; v1.13.0
       go-github-com-jackc-pgx-v4               ; v4.17.2
       go-github-com-microcosm-cc-bluemonday    ; v1.0.22
       go-github-com-miekg-dns                  ; v1.1.50
       go-github-com-minio-minio-go-v7          ; v7.0.48
       go-github-com-mitchellh-mapstructure     ; v1.5.0
       go-github-com-oklog-ulid                 ; v1.3.1
       go-github-com-spf13-cobra                ; v1.6.1
       go-github-com-spf13-viper                ; v1.15.0
       go-github-com-stretchr-testify           ; v1.8.1
       ;github.com/superseriousbusiness/activity v1.2.1-gts
       ;github.com/superseriousbusiness/exif-terminator v0.5.0
       ;github.com/superseriousbusiness/oauth2/v4 v4.3.2-SSB
       ;github.com/tdewolff/minify/v2 v2.12.4
       ;github.com/ulule/limiter/v3 v3.11.0
       ;github.com/uptrace/bun v1.1.10
       ;github.com/uptrace/bun/dialect/pgdialect v1.1.10
       ;github.com/uptrace/bun/dialect/sqlitedialect v1.1.10
       ;github.com/wagslane/go-password-validator v0.3.0
       ;github.com/yuin/goldmark v1.5.4
       ;go.uber.org/automaxprocs v1.5.1
       go-golang-org-x-crypto                   ; v0.6.0
       go-golang-org-x-exp                      ; v0.0.0-20220613132600-b0d781184e0d
       go-golang-org-x-image                    ; v0.5.0
       go-golang-org-x-net                      ; v0.7.0
       ;go-golang-org-x-oauth v0.4.0
       go-golang-org-x-text                     ; v0.7.0
       ;gopkg.in/mcuadros/go-syslog.v2 v2.3.0
       go-gopkg-in-yaml-v3                      ; v0.3.1
       go-modernc-org-sqlite                    ; v1.20.4
       go-mvdan-cc-xurls                        ; v2.4.0
       ))
    (native-inputs
     `(("gotosocial-web-assets.tar.gz"
       ;; Use the pre-generated web assets from upstream instead
       ;; of trying to work with javascript.
        ,(origin
           (method url-fetch)
           (uri (string-append "https://github.com/superseriousbusiness"
                               "/gotosocial/releases/download/v" version
                               "/gotosocial_" version "_web-assets.tar.gz"))
           (sha256
            (base32 "0k0i3qw89fq6w2akdbrbg4s3amp5hznr2b5z5dzz2jragvb8a6yx"))))))
    (home-page "https://docs.gotosocial.org/")
    (synopsis "ActivityPub server powered by Go")
    (description
     "GoToSocial is an @@url{https://activitypub.rocks/,ActivityPub} social
network server, written in Golang.")
    (license (list
               license:silofl1.1    ; web/assets/{Fork_Awesome,NotoSans*.ttf}
               license:agpl3))))

(define-public gotosocial-with-newer-go-libraries
  (package
    (inherit (newer-go-libraries gotosocial))
    (name "gotosocial-with-newer-go-libraries")))



