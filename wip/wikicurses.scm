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

(define-module (wip wikicurses)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public wikicurses
  (package
    (name "wikicurses")
    (version "1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ids1024/wikicurses/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1yxgafk1sczg1xi2p6nhrvr3hchp7ydw98n48lp3qzwnryn1kxv8"))))
    (build-system python-build-system)
    (inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-lxml" ,python-lxml)
       ("python-urwid" ,python-urwid)))
    (home-page "https://github.com/ids1024/wikicurses/")
    (synopsis "Cimple curses interface for MediaWiki sites such as Wikipedia")
    (description "Simple curses interface for MediaWiki sites such as Wikipedia")
    (license license:expat))) ; double-check this
