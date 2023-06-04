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

(define-module (dfsg main tuba)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

(define-public tuba
  (package
    (name "tuba")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GeopJr/Tuba")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xhyz6wi17g4m76lr6qc75q4xnnw7c3dh3d04dg8m5gzk6j0y89x"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags (list "-Ddistro=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'glib-or-gtk-wrap 'symlink-package
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion
               (string-append (assoc-ref outputs "out") "/bin")
               (symlink "dev.geopjr.Tuba" "tuba")))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin") ; for glib-compile-resources
           pkg-config))
    (inputs
     (list gtk
           gtksourceview
           json-glib
           libadwaita
           libgee
           libsoup-minimal
           libsecret
           libwebp
           libxml2
           vala))
    (home-page "https://tuba.geopjr.dev/")
    (synopsis "GTK client for Mastodon")
    (description "Tuba is a GTK client for Mastodon.  It provides a clean,
native interface that allows you to integrate Mastodon's social experience
seamlessly with your desktop environment.")
    (license license:gpl3)))
