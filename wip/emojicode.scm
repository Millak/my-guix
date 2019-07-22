;;; Copyright © 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip emojicode)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python))

(define-public emojicode
  (package
    (name "emojicode")
    (version "0.9")
    (source
      (origin
        (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/emojicode/emojicode")
                 (commit (string-append "v" version))))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0wcfip96m749zglv62rgd9bagn1450fqkfxcrffxyq1hxyrrz39x"))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "tests"
       ;#:configure-flags '((string-append "-DdefaultPackagesDirectory=" ...something
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (invoke "ninja" "magicinstall")
               ;(install-file "emojicode" bin)
               ;(install-file "emojicodec" bin)
               ;(copy-recursively
               ;  (string-append "Emojicode-" ,version "-Linux-"
               ;                 (car (string-split ,(%current-system) #\-))
               ;                 "/packages")
               ;  (string-append out "/lib/emojicode/"))
               )
             #t)))))
    (native-inputs
     `(;("gcc" ,gcc-7)
       ;("gcc:lib" ,gcc-7 "lib")
       ("clang" ,clang-6)
       ("llvm" ,llvm-7)
       ("python" ,python))) ; for the tests
    (home-page "https://www.emojicode.org/")
    (synopsis "World's only programming language that's bursting with emojis")
    (description
     "Emojicode is an open source, high-level, multi-paradigm programming
language consisting of emojis.  It features Object-Orientation, Optionals,
Generics and Closures.")
    (license license:artistic2.0)))
