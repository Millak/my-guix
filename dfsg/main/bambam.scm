;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main bambam)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages python))

(define-public bambam
  (package
    (name "bambam")
    (version "0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/porridge/bambam/archive/"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "10w110mjdwbvddzihh9rganvvjr5jfiz8cs9n7w12zndwwcc3ria"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (add-before 'install 'patch-data-dir-location
           (lambda _
             (substitute* "bambam.py"
                          (("'data'")
                           "'../share/bambam/data'"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share")))
               (mkdir-p bin)
               (copy-file "bambam.py" (string-append bin "/bambam"))
               (install-file "bambam.6" (string-append share "/man/man6"))
               (copy-recursively "data" (string-append share "/bambam/data")))
             #t))
         (add-after 'install 'wrap-binary
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/bambam")))
               (wrap-program bin
                 `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))))
             #t)))))
    (inputs
     `(("python-pygame" ,python-pygame)))
    (home-page "https://github.com/porridge/bambam")
    (synopsis "keyboard mashing and doodling game for babies")
    (description "Bambam is a simple baby keyboard (and gamepad) masher
application that locks the keyboard and mouse and instead displays bright
colors, pictures, and sounds.")
    (license license:gpl3+)))
