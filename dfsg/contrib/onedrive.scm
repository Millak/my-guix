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
  #:use-module (gnu packages sqlite)
  #:use-module (srfi srfi-26))

(define-public onedrive
  (package
    (name "onedrive")
    (version "2.4.21")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/abraunegg/onedrive")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "04rnkc6ap9mkghvlj102f2gvnjqg3bs4vw9q3wm869fsflnm3599"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:modules '((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-26))
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
         (add-after 'unpack 'link-to-external-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "DCFLAGS"
                     (string-join
                       (map (compose dirname (cut search-input-file inputs <>))
                            (list "/lib/libcurl.so.4"
                                  "/lib/libsqlite3.so.0"
                                  ;; These ones for libnotify.
                                  "/lib/libnotify.so.4"
                                  "/lib/libgdk_pixbuf-2.0.so.0"
                                  "/lib/libgio-2.0.so.0"
                                  "/lib/libgobject-2.0.so.0"
                                  "/lib/libglib-2.0.so.0"))
                       " -L-Wl,--rpath=" 'prefix))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./onedrive" "--version")))))))
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
