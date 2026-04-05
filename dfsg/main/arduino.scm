;;; Copyright © 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main arduino)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
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
                            #+go-1.24
                            #+tar
                            #+bzip2
                            #+gzip)))
              (set-path-environment-variable "PATH" '("/bin") inputs))
            ((assoc-ref gnu:%standard-phases 'unpack) #:source #$src)

            (setenv "GOCACHE" "/tmp/gc")
            (setenv "GOMODCACHE" "/tmp/gmc")
            (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs/"))

            (invoke "go" "mod" "vendor")

            (invoke "tar" "czvf" #$output
                    ;; Avoid non-determinism in the archive.
                    "--mtime=@0"
                    "--owner=root:0"
                    "--group=root:0"
                    "--sort=name"
                    "--hard-dereference"
                    ".")))
      #:hash hash-value
      #:hash-algo hash-algorithm)))

(define-public arduino-cli
  (package
    (properties
     '((release-commit . "e39419312d7fb5fb7bf07d8e70c7d18a031ff464")
       (release-date . "2026-01-19T16:11:40Z")))
    (name "arduino-cli")
    (version "1.4.1")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/arduino/arduino-cli")
                    (commit (string-append "v" version))
                    (hash
                     (base32
                      "07bl0ykcmx81pfcab4yy195qmcd5g376hlggjvpv42zs53piavfs"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p2dsbc64432pgc8rszny34wgak663zcr9ica5v23qriz5aaryx6"))))
    (build-system go-build-system)
    (arguments
     (list
       #:go go-1.24
       #:install-source? #f
       #:modules
       '((guix build go-build-system)
         (guix build utils)
         (ice-9 match))
       #:test-flags
       #~(list "-skip" (string-join
                         (list "TestRunScript"
                               "TestDummyMonitor"
                               ;; Needs network
                               "TestDownloadAndChecksums"
                               "TestParseReferenceCores"
                               "TestParseArgs"
                               "TestBuildInjectedInfo")
                         "|"))
       #:import-path "github.com/arduino/arduino-cli"
       #:build-flags
       #~(list (string-append "-ldflags=-X github.com/arduino/arduino-cli/internal/version.versionString="
                              "v" #$version
                              " -X github.com/arduino/arduino-cli/internal/version.commit="
                              (string-take #$(assoc-ref properties 'release-commit) 7)
                              " -X github.com/arduino/arduino-cli/internal/version.date="
                              #$(assoc-ref properties 'release-date)))
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'check 'pre-check
             (lambda* (#:key import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 ;; These tests depend on being run from a git repository.
                 (delete-file-recursively "internal/integrationtest"))
               (setenv "HOME" (getcwd))))
           (add-after 'install 'install-shell-completions
             (lambda* (#:key native-inputs #:allow-other-keys)
               (for-each
                 (match-lambda
                   ((shell . path)
                    (mkdir-p (in-vicinity #$output (dirname path)))
                    (let ((binary
                            (if #$(%current-target-system)
                                (search-input-file native-inputs "bin/arduino-cli")
                                (in-vicinity #$output "bin/arduino-cli"))))
                      (with-output-to-file (in-vicinity #$output path)
                                           (lambda _
                                             (invoke binary "completion" shell))))))
                 '(("bash" . "share/bash-completion/completions/arduino-cli")
                   ("fish" . "share/fish/vendor_completions.d/arduino-cli.fish")
                   ("zsh"  . "share/zsh/site-functions/_arduino-cli"))))))))
    (native-inputs
      (if (%current-target-system)
          (list this-package)
          '()))
    (home-page "https://arduino.github.io/arduino-cli/latest/")
    (synopsis "Arduino command line tool")
    (description "Arduino CLI is an all-in-one solution that provides
Boards/Library Managers, sketch builder, board detection, uploader, and many
other tools needed to use any Arduino compatible board and platform from
command line or machine interfaces.")
    (license license:gpl3)))
