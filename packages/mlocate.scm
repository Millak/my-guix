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

(define-module (packages mlocate)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public mlocate
  (package
    (name "mlocate")
    (version "0.26")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://fedorahosted.org/releases/m/l/mlocate/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0gi6y52gkakhhlnzy0p6izc36nqhyfx5830qirhvk3qrzrwxyqrh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://fedorahosted.org/mlocate/")
    (synopsis "quickly find files on the filesystem based on their name")
    (description "mlocate is a new implementation of locate, a tool to find
files anywhere in the filesystem based on their name, using a fixed pattern or a
regular expression.  Unlike other tools like find(1), locate uses a previously
created database to perform the search, allowing queries to execute much faster.
This database is updated periodically from cron.")
    (license license:gpl2)))
