;;; Copyright © 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages python))

(define-public emojicode
  (package
    (name "emojicode")
    (version "1.0-beta.2")
    (source
      (origin
        (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/emojicode/emojicode")
                 (commit (string-append "v" version))))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "03jbaawmgpbii599hnzj3xrpn7aksmwjdbh4dig93iq4wxh2qahg"))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "tests"
       ;#:build-type "Release"
       ;#:configure-flags '((string-append "-DdefaultPackagesDirectory=" ...something
       #:configure-flags '("-GNinja")
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "ninja" "-j" (number->string (parallel-job-count)))))
         (replace 'check
           (lambda* (#:key tests? test-target #:allow-other-keys)
             (if tests?
               (invoke "ninja" test-target "-j" (number->string (parallel-job-count)))
               (format #t "No tests run\n"))))
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
     `(;("llvm" ,llvm-8)
       ("clang-toolchain" ,clang-toolchain-8)
       ("ncurses" ,(@ (gnu packages ncurses) ncurses)) ; needed?
       ("ninja" ,ninja)
       ("python" ,python)
       ("zlib" ,zlib)
       ("zlib" ,zlib "static")))
    ;(native-search-paths
    ;  (list (search-path-specification
    ;          (variable "EMOJICODE_PACKAGES_PATH")
    ;          (separator #f)              ;single entry
    ;          (files '("lib/emojicode")))))
    (home-page "https://www.emojicode.org/")
    (synopsis "World's only programming language that's bursting with emojis")
    (description
     "Emojicode is a high-level, multi-paradigm programming language consisting
of emojis.  It features Object-Orientation, Optionals, Generics and Closures.")
    (license license:artistic2.0)))
