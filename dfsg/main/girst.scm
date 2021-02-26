;;; Copyright © 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main girst)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public minesviiper
  (let ((commit "9c828b69056ef0ce84d929a52fe8bdf2bcdebc15")
        (revision "2"))
    (package
      (name "minesviiper")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.gir.st/minesVIiper.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1i85a3z71zh9pixq6mbf6bncdl3s8hsx6zild2w30zgwsx1vr7il"))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:make-flags (list (string-append "CC=" ,(cc-for-target)))
         #:phases
          (modify-phases %standard-phases
            (delete 'configure) ; no configure script
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                  (install-file "mines" bin))
                #t)))))
      (home-page "https://gir.st/mines.html")
      (synopsis "Minesweeper clone with vi keybindings")
      (description
       "minesVIiper is a clone of Minesweeper, which runs in the terminal and
can be controlled by either @code{vi} style keybindings, or the mouse.")
      (license license:gpl3))))

(define-public solvitaire
  (let ((commit "d040dc4d2832058e6289257528f6b9972313af8b")
        (revision "4"))
    (package
      (name "solvitaire")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.gir.st/solVItaire.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "01jmnwi9xwwhfzakha5jwfkxvgky7dyrwx9kpgypkycp1nqsikb8"))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "longtest"
         #:make-flags (list (string-append "CC=" ,(cc-for-target)))
         #:phases
          (modify-phases %standard-phases
            (delete 'configure) ; no configure script
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                  (install-file "sol" bin)
                  (install-file "spider" bin)
                  (install-file "freecell" bin))
                #t)))))
      (home-page "https://gir.st/sol.html")
      (synopsis "Solitaire in your terminal")
      (description
       "Play klondike, spider solitaire and freecell in your unicode terminal.

Supports @code{vi} style keybindings, cursor keys and the mouse.")
      (license license:gpl3))))

(define-public viper
  (let ((commit "edd02980f70019b1d01508467c671282561ed778")
        (revision "2"))
    (package
      (name "viper")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.gir.st/VIper.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0lf2ba3grl3i5m6k1hsamwz288sslhy7ydnrwc6gqjc7yl0jm5ab"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no test target
         #:make-flags (list (string-append "CC=" ,(cc-for-target)))
         #:phases
          (modify-phases %standard-phases
            (delete 'configure) ; no configure script
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                  (install-file "viper" bin))
                #t)))))
      (home-page "https://gir.st/viper.html")
      (synopsis "Terminal + emoji = snek")
      (description
       "VIper - a snake clone for unicode-compatible terminals.")
      (license license:gpl3))))

(define-public viiper
  (deprecated-package "viiper" viper))
