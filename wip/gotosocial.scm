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
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages syncthing)
  #:use-module (dfsg main golang))

;;;
;;;
;;;

(define-public gotosocial
  (package
    (name "gotosocial")
    (version "0.9.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/superseriousbusiness"
                                  "/gotosocial/releases/download/v" version
                                  "/gotosocial-" version "-source-code.tar.gz"))
              (sha256
               (base32 "1a1sjm0x758wpwpi3rikbm0ipnr5waxk81zx8rgpp3z9b0n4cy9b"))
              ;(snippet
              ; #~(begin
              ;     (use-modules (guix build utils))
              ;     (delete-file-recursively "vendor")
              ;     ))
              ))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:go ,go-1.20
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
       ;codeberg.org/gruf/go-cache/v3 v3.3.3
       ;codeberg.org/gruf/go-debug v1.3.0
       ;codeberg.org/gruf/go-errors/v2 v2.2.0
       ;codeberg.org/gruf/go-fastcopy v1.1.2
       ;codeberg.org/gruf/go-kv v1.6.1
       ;codeberg.org/gruf/go-logger/v2 v2.2.1
       ;codeberg.org/gruf/go-mutexes v1.1.5
       ;codeberg.org/gruf/go-runners v1.6.1
       ;codeberg.org/gruf/go-sched v1.2.3
       ;codeberg.org/gruf/go-store/v2 v2.2.2

       ;github.com/KimMachineGun/automemlimit v0.2.6
       ;github.com/abema/go-mp4 v0.10.1
       ;github.com/buckket/go-blurhash v1.1.0
       go-github-com-coreos-go-oidc-v3          ; v3.5.0
       go-github-com-disintegration-imaging     ; v1.6.2
       ;github.com/gin-contrib/cors v1.4.0
       ;github.com/gin-contrib/gzip v0.0.6
       ;github.com/gin-contrib/sessions v0.0.5
       ;github.com/gin-gonic/gin v1.9.0
       go-github-com-go-fed-httpsig             ; v1.1.0
       ;github.com/go-playground/form/v4 v4.2.0
       go-github-com-go-playground-validator-v10    ; v10.14.0
       go-github-com-google-uuid                ; v1.3.0
       go-github-com-gorilla-feeds              ; v1.1.1
       go-github-com-gorilla-websocket          ; v1.5.0
       ;github.com/h2non/filetype v1.1.3
       go-github-com-jackc-pgconn               ; v1.14.0
       go-github-com-jackc-pgx-v5               ; v5.3.1
       go-github-com-microcosm-cc-bluemonday    ; v1.0.23
       go-github-com-miekg-dns                  ; v1.1.54
       ;go-github-com-minio-minio-go-v7          ; v7.0.53
       go-github-com-mitchellh-mapstructure     ; v1.5.0
       go-github-com-oklog-ulid                 ; v1.3.1
       go-github-com-spf13-cobra                ; v1.7.0
       go-github-com-spf13-viper                ; v1.15.0
       go-github-com-stretchr-testify           ; v1.8.2
       ;github.com/superseriousbusiness/activity v1.3.0-gts
       ;github.com/superseriousbusiness/exif-terminator v0.5.0
       ;github.com/superseriousbusiness/oauth2/v4 v4.3.2-SSB.0.20230227143000-f4900831d6c8
       go-github-com-tdewolff-minify-v2         ; v2.12.5
       ;github.com/ulule/limiter/v3 v3.11.1
       ;github.com/uptrace/bun v1.1.13
       ;github.com/uptrace/bun/dialect/pgdialect v1.1.13
       ;github.com/uptrace/bun/dialect/sqlitedialect v1.1.13
       ;github.com/uptrace/bun/extra/bunotel v1.1.12
       ;github.com/wagslane/go-password-validator v0.3.0
       go-github-com-yuin-goldmark              ; 1.5.4

       ;go.opentelemetry.io/otel v1.14.0
       ;go.opentelemetry.io/otel/exporters/jaeger v1.14.0
       ;go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracegrpc v1.14.0
       ;go.opentelemetry.io/otel/sdk v1.14.0
       ;go.opentelemetry.io/otel/trace v1.14.0
       ;go.uber.org/automaxprocs v1.5.2
       go-golang-org-x-crypto-0.9               ; v0.9.0
       go-golang-org-x-exp-0.0.0-20220613132600-b0d781184e0d
       go-golang-org-x-image-0.7                ; v0.7.0
       go-golang-org-x-net-0.10                 ; v0.10.0
       go-golang-org-x-oauth2-0.8               ; 0.8.0
       go-golang-org-x-text-0.9                 ; v0.9.0
       ;gopkg.in/mcuadros/go-syslog.v2 v2.3.0
       go-gopkg-in-yaml-v3                      ; v3.0.1
       go-modernc-org-sqlite                    ; v1.22.1
       go-mvdan-cc-xurls                        ; v2.5.0
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
            (base32 "1yxyl8q0dqpv3cqzxj0anh8rskk3rmw9mjnjkm4svc0vgaph1y1s"))))))
    (home-page "https://docs.gotosocial.org/")
    (synopsis "ActivityPub server powered by Go")
    (description
     "GoToSocial is an @@url{https://activitypub.rocks/,ActivityPub} social
network server, written in Golang.")
    (license (list
               license:silofl1.1    ; web/assets/{Fork_Awesome,NotoSans*.ttf}
               license:agpl3))))

(define-public with-gotosocial-deps
  (package-input-rewriting/spec
    `(
      ;; This one is in Guix twice.
      ("go-github.com-mattn-go-runewidth" . ,(const go-github-com-mattn-go-runewidth))
      ;; This isn't picked up for some reason.
      ;("go-github-com-hashicorp-go-version" . ,(const go-github-com-hashicorp-go-version-1.3.0))
      ;; We should use the newer versions.
      ("go-golang-org-x-crypto" . ,(const go-golang-org-x-crypto-0.9))
      ("go-golang-org-x-exp" . ,(const go-golang-org-x-exp-0.0.0-20220613132600-b0d781184e0d))
      ("go-golang-org-x-image" . ,(const go-golang-org-x-image-0.7))
      ;("go-golang-org-x-lint" . ,(const go-golang-org-x-lint-next))
      ;("go-golang-org-x-mod" . ,(const go-golang-org-x-mod-next))
      ("go-golang-org-x-net" . ,(const go-golang-org-x-net-0.10))
      ("go-golang-org-x-oauth2" . ,(const go-golang-org-x-oauth2-0.8))
      ;("go-golang-org-x-sync" . ,(const go-golang-org-x-sync-next))
      ("go-golang-org-x-term" . ,(const go-golang-org-x-term-0.8))
      ("go-golang-org-x-text" . ,(const go-golang-org-x-text-0.9))
      ;("go-golang-org-x-time" . ,(const go-golang-org-x-time-next))
      ("go-golang-org-x-tools" . ,(const go-golang-org-x-tools-0.6))
      ;("go-golang-org-x-xerrors" . ,(const go-golang-org-x-xerrors-next))
      )))

(define-public gotosocial-with-newer-go-libraries
  (package
    (inherit (with-gotosocial-deps gotosocial))
    (name "gotosocial-with-newer-go-libraries")))
