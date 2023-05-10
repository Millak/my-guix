;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main seashells)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages video))

(define-public seashells
  (let ((commit "119f3d7c897a3028e08a638f870d83ca0bddb8f3") ;2023-03-05
        (revision "1"))
    (package
      (name "seashells")
      (version (git-version "0.1.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/anishathalye/seashells")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "019d0np7gb6k392q7jr73vlw8d1krdzlbwsr6hj5x6pagqdw1ncl"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f)) ;No tests
      (home-page "https://seashells.io/")
      (synopsis "Official client for seashells.io")
      (description
       "Seashells lets you pipe output from command-line programs
to the web in real-time, even without installing any new software on your
machine.  You can use it to monitor long-running processes like experiments
that print progress to the console.")
      (license license:expat))))
