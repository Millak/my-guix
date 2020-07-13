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

(define-module (dfsg main minesviiper)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public minesviiper
  (let ((commit "04f8849175609019b0a136705ee43c1c5e1a2b8e")
        (revision "1"))
    (package
      (name "minesviiper")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/girst/minesviiper")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "138w0sjy00swldx3yjrqbmfjjk77520n6mf29p8br2gpcxf1ivdg"))))
      (build-system gnu-build-system)
      (arguments
       '(#:test-target "test_long"
         #:make-flags (list "CC=gcc")
         #:phases
          (modify-phases %standard-phases
            (delete 'configure) ; no configure script
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                  (install-file "mines" bin))
                #t)))))
      (home-page "https://gir.st/mines.htm")
      (synopsis "Minesweeper clone with vi keybindings")
      (description
       "minesVIiper is a clone of Minesweeper, which runs in the terminal and
can be controlled by either @code{vi} style keybindings, or the mouse.")
      (license license:gpl3))))
