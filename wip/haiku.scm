;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip haiku)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bison))

(define haiku-buildtools-source
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://review.haiku-os.org/buildtools")
           (commit "r1beta2")))
    (file-name (git-file-name "buildtools" "r1beta2"))
    (sha256
     (base32
      "05syrzvnq9xw4hvl5vapcqixdrd08vvhczlcwz0648np1iq2cxim"))))

(define haiku-source
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://review.haiku-os.org/haiku")
           (commit "r1beta2")))
    (file-name (git-file-name "haiku" "r1beta2"))
    (sha256
     (base32
      "1qfw1z2n0di88y77ii9v8y48chny05qf7ag5mxa00dsj78q6yy17"))))

;; GENERATED = jamgram.c jamgram.h jambase.c jamgramtab.h
(define-public jam
  (package
    (name "jam")
    (version "r1beta2")
    (source haiku-buildtools-source)
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'change-directory
           (lambda _
             (for-each make-file-writable (find-files "jam"))
             (chdir "jam")
             #t))
         (add-after 'change-directory 'patch-source
           (lambda _
             (setenv "CC" (which "gcc"))
             (substitute* "Makefile"
               (("CC = cc") "CC = gcc"))
             (substitute* "execunix.c"
               (("# define USE_POSIX_SPAWN") ""))
             (mkdir-p "bin.linuxx86")
             #t))
         ;(replace 'build
         ;  (lambda _
         ;    (invoke "make" "LINKLIBS=" "./jam0")
         ;    ;(for-each delete-file '("jamgram.c" "jamgram.h" "jambase.c" "jamgramtab.h"))
         ;    (for-each make-file-writable (find-files "."))
         ;    (invoke "./jam0" "-d" "x")
         ;    ))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
             (invoke "./jam0"
                     (string-append "-sBINDIR="
                                    (assoc-ref outputs "out") "/bin")
                     "install")
             ))
         )))
    (native-inputs
     `(
       ("bison" ,bison)
       ))
    (home-page "https://www.haiku-os.org/guides/building/jam")
    (synopsis "Software-build tool, replacement for make")
    (description "Jam's primary feature was its ability to express build
patterns in an imperative language which supported structured namespaces
(similar to Pascal records) and simple lists.")
    (license license:expat)))  ; I think? Need to find a good source.
