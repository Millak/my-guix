;;; Copyright © 2016, 2017, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main ecrire)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config))

(define-public ecrire
  (package
    (name "ecrire")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/"
                            "apps/ecrire/ecrire-" version ".tar.xz"))
        (sha256
         (base32 "1pszk583rzclfqy3dyjh1m9pz1hnr84vqz8vw9kngcnmj23mjr6r"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list efl))
    (home-page "https://www.enlightenment.org")
    (synopsis "Text editor for EFL/Enlightenment")
    (description "Ecrire is a basic text editor written in EFL for the
Enlightenment desktop environment and also Tizen.  It is intended to be a native
EFL alternative to gedit (GTK/Gnome), kwrite (KDE/Plasma), and similar basic
text editors.")
    (license license:gpl3)))

;(define-public ecrire
;  (package
;    (name "ecrire")
;    (version "0.3.4")
;    (source
;      (origin
;        (method git-fetch)
;        (uri (git-reference
;               (url "https://github.com/Obsidian-StudiosInc/ecrire")
;               (commit (string-append "v" version))))
;        (file-name (git-file-name name version))
;        (sha256
;         (base32
;          "05aiadyjsyxj35wks7y3k6wjvgycwf4f7jbpxns8lr6ab64yzvk8"))))
;    (build-system cmake-build-system)
;    (arguments
;     '(#:configure-flags '("-DENABLE_NLS:BOOL=TRUE")
;       #:tests? #f))    ; no tests
;    (native-inputs
;     (list gettext-minimal
;           pkg-config))
;    (inputs
;     (list efl))
;    (home-page "https://www.enlightenment.org")
;    (synopsis "Text editor for EFL/Enlightenment")
;    (description "Ecrire is a basic text editor written in EFL for the
;Enlightenment desktop environment and also Tizen.  It is intended to be a native
;EFL alternative to gedit (GTK/Gnome), kwrite (KDE/Plasma), and similar basic
;text editors.")
;    (license license:gpl3)))
