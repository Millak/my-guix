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

(define-module (dfsg main solvitaire)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public solvitaire
  (let ((commit "26738c8a3404a1431dd61ca306f013cd07dd2418")
        (revision "1"))
    (package
      (name "solvitaire")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/girst/solvitaire.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "12x2fy1cw3np9rq3s1jwffkyi80chv0brlxr1q50ig8vw3k87hrs"))))
      (build-system gnu-build-system)
      (arguments
       '(#:test-target "longtest"
         #:make-flags (list "CC=gcc")
         #:phases
          (modify-phases %standard-phases
            (delete 'configure) ; no configure script
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                  (install-file "sol" bin)
                  (install-file "spider" bin))
                #t)))))
      (home-page "https://gir.st/sol.htm")
      (synopsis "solitaire in your terminal")
      (description
       "Play klondike and spider solitaire in your unicode terminal.

Supports @code{vi} style keybindings, cursor keys and the mouse.")
      (license license:gpl3))))
