;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is an addendum GNU Guix.
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

(define-module (packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (srfi srfi-1))

;; According to https://golang.org/doc/install/gccgo, gccgo-4.8.2 includes a
;; complete go-1.1.2 implementation, gccgo-4.9 includes a complete go-1.2
;; implementation, and gccgo-5 a complete implementation of go-1.4.  Ultimately
;; we hope to build go-1.5+ with a bootstrap process using gccgo-5.  As of
;; go-1.5, go cannot be bootstrapped without go-1.4, so we need to use go-1.4 or
;; gccgo-5.  Mips is not officially supported, but it should work if it is
;; bootstrapped.

(define go-1.4
  (package
    (name "go")
    (version "1.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://storage.googleapis.com/golang/go"
                            version ".src.tar.gz"))
        (sha256
         (base32
          "0na9yqilzpvq0bjndbibfp07wr796gf252y471cip10bbdqgqiwr"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc" "tests"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'patch-generated-file-shebangs 'chdir
           (lambda _ (chdir "src")))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((gccgo  (assoc-ref inputs "gccgo"))
                   (output (assoc-ref outputs "out")))
               (setenv "CC" "gcc")
               (setenv "GOROOT" (getcwd))
               (setenv "GOROOT_BOOTSTRAP" gccgo)
               (setenv "GOROOT_FINAL" output)
               (setenv "CGO_ENABLED" "0")
               ;; all.bash includes the tests, which fail because they
               ;; require network access, and access to /bin
               (zero? (system* "bash" "make.bash")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (docs   (assoc-ref outputs "doc"))
                    (tests  (assoc-ref outputs "tests")))
               (copy-recursively "../test" tests)
               (delete-file-recursively "../test")
               (copy-recursively "../api" (string-append docs "/api"))
               (delete-file-recursively "../api")
               (copy-recursively "../doc" (string-append docs "/doc"))
               (delete-file-recursively "../doc")
               (copy-recursively "../" output)))))
       #:tests? #f))
    (native-inputs
     `(("gccgo" ,gccgo-4.9)
       ("perl" ,perl)))
    (home-page "https://golang.org/")
    (synopsis "Compiled, statically typed language developed by Google")
    (description "Go, also commonly referred to as golang, is a programming
 language developed at Google.  Designed primarily for systems programming, it
 is a compiled, statically typed language in the tradition of C and C++, with
garbage collection, various safety features and in the style of communicating
sequential processes (CSP) concurrent programming features added.")
    (license license:bsd-3)))

(define-public go
  (package (inherit go-1.4)
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://storage.googleapis.com/golang/go"
                            version ".src.tar.gz"))
        (sha256
         (base32
          "1k5wy5ijll5aacj1m6xxnjfjw6x9f255ml3f1jiicw031mshnyvq"))))
    (native-inputs
     `(("gccgo" ,go-1.4)
       ,@(alist-delete "gccgo" (package-native-inputs go-1.4))))))
