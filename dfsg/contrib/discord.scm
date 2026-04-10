;;; Copyright © 2024 umanwizard <brennan@umanwizard.com>
;;; Copyright © 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg contrib discord)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
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
                            #+go
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

(define-public dissent
  (package
    (name "dissent")
    (version "0.0.37")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/diamondburned/dissent")
                    (commit (string-append "v" version))
                    (hash
                     (base32
                      "1xg40mbia8gi1z5037mdmkyixvqdvb7rsv68sdpj4rjcnqq5dcy6"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b8n07c4k6wjvi7l78jyqygh7kr22hnsc5wfjdmxjjapprwsrcdc"))))
    (build-system go-build-system)
    (arguments
     (list
       #:go go-1.24
       #:install-source? #f
       #:import-path "libdb.so/dissent"
       #:imported-modules (append %glib-or-gtk-build-system-modules
                                  %go-build-system-modules)
       #:modules
       '((guix build utils)
         (guix build go-build-system)
         ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'glib-or-gtk-wrap
             (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (inputs
     (list gobject-introspection
           gtk
           gtksourceview
           libadwaita
           libcanberra
           libspelling))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/diamondburned/dissent")
    (synopsis "Tiny native Discord app")
    (description "Dissent (previously gtkcord4) is a third-party Discord client
designed for a smooth, native experience on Linux desktops.  Built with the
GTK4 and libadwaita for a modern look and feel, it delivers your favorite
Discord app in a lightweight and visually appealing package.")
    (license license:gpl3+)))
