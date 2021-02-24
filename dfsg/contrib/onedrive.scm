;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg contrib onedrive)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite))

;; More recent versions need newer versions of D.
(define-public onedrive
  (package
    (name "onedrive")
    (version "2.3.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/abraunegg/onedrive")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1f9i86izvmylch03wjh2lf4dy9k6777w3is9f416w31nxs90rxpw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f      ; No tests
       #:configure-flags '("--enable-completions"
                           ;,(string-append "--with-zsh-completion-dir="
                           ;                (assoc-ref %outputs "out")
                           ;                "/share/zsh/site-functions")
                           "--enable-notifications")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1"))
                    (zsh (string-append out "/share/zsh/site-functions"))
                    (bash (string-append out "/etc/bash_completion.d"))
                    )
               (install-file "onedrive" bin)
               (install-file "onedrive.1" man1)
               (mkdir-p zsh)
               (copy-file "contrib/completions/complete.zsh"
                          (string-append zsh "/_onedrive"))
               (mkdir-p bash)
               (copy-file "contrib/completions/complete.bash"
                          (string-append bash "/onedrive"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl-minimal)
       ("ldc" ,ldc)
       ("libnotify" ,libnotify)
       ("sqlite" ,sqlite)))
    (home-page "https://abraunegg.github.io")
    (synopsis "Unofficial OneDrive Client")
    (description "OneDrive Client which supports OneDrive Personal, OneDrive for
Business, OneDrive for Office365 and SharePoint and fully supports Azure
National Cloud Deployments.  It supports one-way and two-way sync capabilities
and securely connects to Microsoft OneDrive services.")
    (license license:gpl3)))
