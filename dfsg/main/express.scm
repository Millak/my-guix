;;; Copyright © 2016, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main express)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config))

(define-public express-irc
  (let ((commit "ec9f9c95150da86925b9e3cf0ad9017fc2c8a531")
        (revision "3"))
    (package
      (name "express-irc")
      (version (git-version "0.0.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.enlightenment.org/apps/express.git/")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1c8dgz975ng2vlcb4dj8d2m90fam9m5c7xx7lv64yn5ppwh0qgq2"))
          (modules '((guix build utils)))
          (snippet
           '(begin
              (delete-file-recursively "data/fonts")
              (substitute* "meson.build"
                ((".*data/fonts.*") ""))
              #t))))
      (build-system meson-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'setenv
             (lambda _
               ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
               (setenv "HOME" "/tmp")
               #t)))))
      (native-inputs
       `(("gettext" ,gettext-minimal)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("efl" ,efl)))
      (home-page "https://git.enlightenment.org/apps/express.git/")
      (synopsis "IRC client with enhanced media capabilities")
      (description "EFL-based IRC Client which operates similar to the
Terminology interface.")
      (license license:bsd-2))))
