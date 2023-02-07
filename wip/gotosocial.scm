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
    (version "0.6.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/superseriousbusiness"
                                  "/gotosocial/releases/download/v" version
                                  "/gotosocial-" version "-source-code.tar.gz"))
              (sha256
               (base32 "1ywfq6dvqc6nhyv51sjcbk6kj99m0d6h3ii543fk815a9n6236g0"))
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
                          "-count" "1" "./...")))))
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
                 (invoke "tar" "xvf" web)))))
       ;  (add-after 'install 'wrap-program
       ;    (lambda* (#:key outputs inputs #:allow-other-keys)
       ;      (let* ((out (assoc-ref outputs "out"))
       ;             (bin (string-append out "/bin/gotosocial"))
       ;             (git (assoc-ref inputs "git-minimal"))
       ;             (ssh (assoc-ref inputs "openssh-sans-x")))
       ;        (wrap-program bin
       ;          `("PATH" ":" prefix (,(string-append git "/bin")
       ;                                ,(string-append ssh "/bin")))))))
       )
       ))
    (inputs
     (list
       sqlite
       ;codeberg.org/gruf/go-bytesize v1.0.0
       ;codeberg.org/gruf/go-byteutil v1.0.2
       ;codeberg.org/gruf/go-cache/v3 v3.1.8
       ;codeberg.org/gruf/go-debug v1.2.0
       ;codeberg.org/gruf/go-errors/v2 v2.0.2
       ;codeberg.org/gruf/go-kv v1.5.2
       ;codeberg.org/gruf/go-logger/v2 v2.2.1
       ;codeberg.org/gruf/go-mutexes v1.1.4
       ;codeberg.org/gruf/go-runners v1.3.1
       ;codeberg.org/gruf/go-store/v2 v2.0.10
       ;github.com/buckket/go-blurhash v1.1.0
       go-github-com-coreos-go-oidc
       ;github.com/cornelk/hashmap v1.0.8
       go-github-com-disintegration-imaging
       ;github.com/gin-contrib/cors v1.4.0
       ;github.com/gin-contrib/gzip v0.0.6
       ;github.com/gin-contrib/sessions v0.0.5
       ;github.com/gin-gonic/gin v1.8.1
       ;github.com/go-fed/httpsig v1.1.0
       go-gopkg-in-go-playground-validator-v9
       ;github.com/go-playground/validator/v10 v10.11.1
       go-github-com-google-uuid
       ;github.com/gorilla/feeds v1.1.1
       go-github-com-gorilla-websocket
       ;github.com/h2non/filetype v1.1.3
       ;go-github-com-jackc-pgconn          ; FTBFS
       ;go-github-com-jackc-pgx-v4          ; FTBFS
       go-github-com-microcosm-cc-bluemonday
       go-github-com-miekg-dns
       go-github-com-minio-minio-go-v7
       go-github-com-mitchellh-mapstructure
       go-github-com-oklog-ulid
       go-github-com-robfig-cron
       go-github-com-russross-blackfriday
       go-github-com-spf13-cobra
       go-github-com-spf13-viper
       go-github-com-stretchr-testify
       ;github.com/superseriousbusiness/activity v1.2.1-gts
       ;github.com/superseriousbusiness/exif-terminator v0.4.0
       ;github.com/superseriousbusiness/oauth2/v4 v4.3.2-SSB
       ;github.com/tdewolff/minify/v2 v2.12.4
       ;github.com/ulule/limiter/v3 v3.10.0
       ;github.com/uptrace/bun v1.1.9
       ;github.com/uptrace/bun/dialect/pgdialect v1.1.9
       ;github.com/uptrace/bun/dialect/sqlitedialect v1.1.9
       ;github.com/wagslane/go-password-validator v0.3.0
       go-golang-org-x-crypto
       go-golang-org-x-exp
       go-golang-org-x-net
       ;go-golang-org-x-oauth
       go-golang-org-x-text
       ;gopkg.in/mcuadros/go-syslog.v2 v2.3.0
       go-gopkg-in-yaml-v3
       ;go-modernc-org-sqlite           ; FTBFS
       go-mvdan-cc-xurls
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
            (base32 "17sfny68wx3a16sl1fkfxvr7225nyaf0d4r4gw1phis65l7cj1s4"))))))
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



