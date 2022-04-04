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

(define-module (dfsg contrib onedrive)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite))

(define-public onedrive
  (package
    (name "onedrive")
    (version "2.4.16")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/abraunegg/onedrive")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "14hp4x32nbg0ix9h8z8rx3pvzmz2w8s744gd2qv2iz5fq08rz2qs"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:tests? #f              ; Tests require a OneDrive account.
       #:validate-runpath? #f   ; We know it fails, hence the LD_PRELOAD.
       #:configure-flags
       #~(list "--enable-completions"
               "--enable-notifications"
               (string-append "--with-zsh-completion-dir="
                              #$output "/share/zsh/site-functions")
               (string-append "--with-fish-completion-dir="
                              #$output "/share/fish/vendor_completions.d"))
       #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append #$output "/bin/onedrive")
               `("LD_PRELOAD" ":" prefix
                 (,(search-input-file inputs "/lib/libcurl.so.4")
                   ,(search-input-file inputs "/lib/libsqlite3.so.0")
                   ;; These ones for libnotify
                   ,(search-input-file inputs "/lib/libnotify.so.4")
                   ,(search-input-file inputs "/lib/libgdk_pixbuf-2.0.so.0")
                   ,(search-input-file inputs "/lib/libgio-2.0.so.0")
                   ,(search-input-file inputs "/lib/libgobject-2.0.so.0")
                   ,(search-input-file inputs "/lib/libglib-2.0.so.0")))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list bash-minimal
           binutils-gold    ; Apparently really does want ld.gold
           curl-minimal
           ldc
           libnotify
           sqlite))
    (home-page "https://abraunegg.github.io")
    (synopsis "Client for OneDrive")
    (description "OneDrive Client which supports OneDrive Personal, OneDrive for
Business, OneDrive for Office365 and SharePoint and fully supports Azure
National Cloud Deployments.  It supports one-way and two-way sync capabilities
and securely connects to Microsoft OneDrive services.")
    (license license:gpl3)))
