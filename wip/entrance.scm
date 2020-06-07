;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip entrance)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public entrance
  (package
    (name "entrance")
    (version "3.0.0_alpha6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Obsidian-StudiosInc/entrance")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1kr4d2cj6llkhqxj0xx9qdd7ghma06z2ssi6mn0rg00jha77krj7"))))
    (build-system meson-build-system)
    (arguments
     '(#:tests? #f  ; no tests
       #:configure-flags '("-Dlogind=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((pam (assoc-ref inputs "pam"))
                   (out (assoc-ref outputs "out")))
               (substitute* "meson.build"
                 (("/usr/include/security")
                  (string-append pam "/include/security"))
                 (("/usr/lib/systemd/system")
                  (string-append out "/lib/systemd/system")))
               #t)))
         (add-after 'unpack 'set-home-dir
           (lambda _
             (setenv "HOME" (getenv "TMPDIR"))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)
       ("pam" ,linux-pam)))
    (home-page "https://github.com/Obsidian-StudiosInc/entrance")
    (synopsis "Display Manager built on EFL")
    (description "Entrance is a Unix Display/Login Manager written in
Enlightenment Foundation Libraries (EFL).  Entrance allows a user to choose an X
WM/Desktop session to launch upon successful login.  Entrance is alive and
working again for logging into X sessions and eventually Wayland sessions!

The project has been resurrected from the dead to live on once again...")
    (license license:gpl3)))

(define-public entrance-git
  (let ((commit "2c02d1b1d5c7221059f81c5b6c89fe71f23640cc")
        (revision "1"))
    (package
      (inherit entrance)
      (name "entrance-git")
      (version (git-version (package-version entrance) revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/Obsidian-StudiosInc/entrance")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "172wvlw0fkmqw6s72izgi9jnpjfhx8r0n7v1b71n3gj0sddglvma")))))))
