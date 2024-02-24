;;; Copyright © 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip gitea)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages version-control)
  #:use-module (dfsg main golang))

;;;
;;;
;;;

(define-public gitea
  (package
    (name "gitea")
    (version "1.15.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/go-gitea/gitea/releases"
                                  "/download/v" version
                                  "/gitea-src-" version ".tar.gz"))
              (sha256
               (base32 "0ihw68qy36xdwp6kiardxlbp1x0s10gjdkg51b6p93c0r9pm9501"))
              (patches (search-patches "gitea-patch-makefile.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; TODO: Download using git-fetch and unvendor javascript.
                  (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:import-path "code.gitea.io/gitea"
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'unpatch-example-shebangs
           ;; If we don't do this then git repos created with this version of
           ;; gitea will use the build environment's bash for the different
           ;; git repo hooks.
           (lambda _
             (substitute*
               (find-files "src/integrations/gitea-repositories-meta"
                           "(\\.sample|gitea|(post|pre)-receive|update)")
               (("#!/gnu/store/.*/bin/bash") "#!/bin/bash")
               (("#!/gnu/store/.*/bin/sh") "#!/bin/sh"))))
         (add-before 'build 'prepare-build
           (lambda _
             (setenv "TAGS" "bindata sqlite sqlite_unlock_notify")))
         (replace 'build
           (lambda _
             (with-directory-excursion "src/code.gitea.io/gitea"
               ;; Upstream suggests to run 'make frontend' before 'make build'.
               (invoke "make" "build")
               (invoke "make" "generate-manpage"))))
         (add-before 'check 'pre-check
           (lambda* (#:key import-path #:allow-other-keys)
             (setenv "GIT_COMMITTER_NAME" "Your Name")
             (setenv "GIT_COMMITTER_EMAIL" "you@example.com")

             (with-directory-excursion "src/code.gitea.io/gitea"
               ;; There is no network available in the build environment.
               (substitute* "modules/migrations/github_test.go"
                 (("TestGitHubDownloadRepo") "Disabled_TestGitHubDownloadRepo"))
               (substitute* "modules/migrations/migrate_test.go"
                 (("TestMigrateWhiteBlocklist") "Disabled_TestMigrateWhiteBlocklist"))

               ;; TODO: Look into these test failures:
               (substitute* "modules/markup/sanitizer_test.go"
                 (("TestSanitizeNonEscape") "Disabled_TestSanitizeNonEscape"))
               (substitute* "modules/markup/html_test.go"
                 (("TestRender_links") "Disabled_TestRender_links")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "src/code.gitea.io/gitea"
                 (setenv "HOME" (mkdtemp "/tmp/home.XXXXXX"))
                 ;; First we start with the unit tests:
                 ;; Skip testing the front-end, we're using the bundled javascript.
                 ;(invoke "make" "test")
                 (invoke "make" "test-backend")
                 ;; Then we continue with the integration tests:
                 ;; "Gitea requires git with lfs support to run tests."
                 ;(invoke "make" "test-sqlite")
                 (invoke "make" "test-sqlite-migration")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "src/code.gitea.io/gitea"
               (invoke "make" "install")
               (install-file "man/man1/gitea.1.gz"
                             (string-append (assoc-ref outputs "out")
                                            "/share/man/man1")))))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/gitea"))
                    (git (assoc-ref inputs "git-minimal"))
                    (ssh (assoc-ref inputs "openssh-sans-x")))
               (wrap-program bin
                 `("PATH" ":" prefix (,(string-append git "/bin")
                                       ,(string-append ssh "/bin"))))))))))
    (native-inputs
     (list go-github-com-stretchr-testify
           node-lts))
    (inputs
     (list bash-minimal
           git-minimal
           openssh-sans-x

           go-cloud-google-com-go
           go-code-gitea-io-gitea-vet
           go-code-gitea-io-sdk-gitea
           go-gitea-com-go-chi-binding
           go-gitea-com-go-chi-cache
           go-gitea-com-go-chi-captcha
           go-gitea-com-go-chi-session
           go-gitea-com-lunny-levelqueue
           go-github-com-alecthomas-chroma
           go-github-com-blevesearch-bleve-v2
           go-github-com-caddyserver-certmagic
           go-github-com-chi-middleware-proxy
           go-github-com-denisenkom-go-mssqldb
           go-github-com-djherbis-buffer
           go-github-com-djherbis-nio-v3
           go-github-com-dustin-go-humanize
           go-github-com-editorconfig-editorconfig-core-go-v2
           go-github-com-emirpasic-gods
           go-github-com-ethantkoenig-rupture
           go-github-com-gliderlabs-ssh
           go-github-com-gobwas-glob
           go-github-com-go-chi-chi
           go-github-com-go-chi-cors
           go-github-com-go-enry-go-enry-v2
           go-github-com-go-git-go-billy-v5
           go-github-com-go-git-go-git-v5
           go-github-com-gogs-chardet
           go-github-com-gogs-cron
           go-github-com-gogs-go-gogs-client
           go-github-com-go-ldap-ldap-v3
           go-github-com-google-go-github-v32
           go-github-com-go-redis-redis-v8
           go-github-com-gorilla-context
           go-github-com-go-sql-driver-mysql
           go-github-com-go-swagger-go-swagger
           go-github-com-go-testfixtures-testfixtures-v3
           go-github-com-hashicorp-golang-lru
           go-github-com-huandu-xstrings
           go-github-com-issue9-identicon
           go-github-com-jaytaylor-html2text
           go-github-com-json-iterator-go
           go-github-com-kballard-go-shellquote
           go-github-com-keybase-go-crypto
           go-github-com-klauspost-compress
           go-github-com-lafriks-xormstore
           go-github-com-lib-pq
           go-github-com-lunny-dingtalk-webhook
           go-github-com-markbates-goth
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-sqlite3
           go-github-com-mholt-archiver-v3
           go-github-com-microcosm-cc-bluemonday
           go-github-com-minio-minio-go-v7
           go-github-com-nfnt-resize
           go-github-com-niklasfasching-go-org
           go-github-com-nytimes-gziphandler
           go-github-com-oliamb-cutter
           go-github-com-olivere-elastic-v7
           go-github-com-pkg-errors
           go-github-com-pquerna-otp
           go-github-com-prometheus-client-golang
           go-github-com-puerkitobio-goquery
           go-github-com-quasoft-websspi
           go-github-com-sergi-go-diff
           go-github-com-shurcool-vfsgen
           go-github-com-syndtr-goleveldb
           go-github-com-tstranex-u2f
           go-github-com-unknwon-com
           go-github-com-unknwon-i18n
           go-github-com-unknwon-paginater
           go-github-com-unrolled-render
           go-github-com-urfave-cli
           go-github-com-xanzy-go-gitlab
           go-github-com-yohcop-openid-go
           go-github-com-yuin-goldmark
           go-github-com-yuin-goldmark-highlighting
           go-github-com-yuin-goldmark-meta
           go-go-jolheiser-com-hcaptcha
           go-go-jolheiser-com-pwn
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-golang-org-x-sys
           go-golang-org-x-text
           go-golang-org-x-tools
           go-gopkg-in-gomail-v2
           go-gopkg-in-ini-v1
           go-gopkg-in-yaml-v2
           go-mvdan-cc-xurls
           go-strk-kbt-io-projects-go-libravatar
           go-xorm-io-builder
           go-xorm-io-xorm

           go-github-com-6543-go-version
           go-github-com-zeripath-jwt))
    (home-page "https://gitea.io/")
    (synopsis "Self-hosted git service")
    (description
     "Gitea (git with a cup of tea) is a painless self-hosted Git Service.")
    (properties
      '((release-monitoring-url . "https://github.com/go-gitea/gitea/releases")))
    (license license:expat)))

;(define-public gitea-with-newer-go-libraries
;  (package
;    (inherit (newer-go-libraries gitea))
;    (name "gitea-with-newer-go-libraries")))
