;;; Copyright © 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages python-build))

(define-public seashells
  (let ((commit "9d80defe43f044ffcb3b3e3ee7423b663a440c0c") ;2024-12-27
        (revision "1"))
    (package
      (name "seashells")
      (version "1.0.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/anishathalye/seashells")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0cr25c013q6qj60s5n1jjf5sfhkbg1ql3n4l5pzpnk1a28rf7ds2"))))
      (build-system pyproject-build-system)
      (arguments
       `(#:tests? #f)) ;No tests
      (native-inputs
       (list python-hatchling))
      (home-page "https://seashells.io/")
      (synopsis "Official client for seashells.io")
      (description
       "Seashells lets you pipe output from command-line programs
to the web in real-time, even without installing any new software on your
machine.  You can use it to monitor long-running processes like experiments
that print progress to the console.")
      (license license:expat))))
