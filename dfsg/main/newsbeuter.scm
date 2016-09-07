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

(define-module (dfsg main newsbeuter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web))

(define-public stfl
  (package
    (name "stfl")
    (version "0.24")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://www.clifford.at/stfl/stfl-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1460d5lc780p3q38l3wc9jfr2a7zlyrcra0li65aynj738cam9yl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         ;; there is no configure script so we get to do it manually
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
                          (("\\$\\(prefix\\)") (assoc-ref outputs "out")))
             (setenv "DESTDIR" "")
             #t))
         ;; in our ncurses, the headers are in /include
         (add-before 'build 'patch-ncursesw
           (lambda _
             (substitute* '("stfl_internals.h")
                          (("ncursesw/") ""))
             #t))
         (add-after 'install 'install-missing-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               ;; newsbeuter looks for libstfl.so.0
               (symlink (string-append lib "/libstfl.so")
                        (string-append lib "/libstfl.so.0"))))))))
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "http://www.clifford.at/stfl/")
    (synopsis "Structured terminal forms library")
    (description "Stfl is a library which implements a curses-based widget
set for text terminals.")
    (license license:lgpl3+)))

(define-public newsbeuter
  (package
    (name "newsbeuter")
    (version "2.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://newsbeuter.org/downloads/newsbeuter-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1j1x0hgwxz11dckk81ncalgylj5y5fgw5bcmp9qb5hq9kc0vza3l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "config.sh"
                          ;; ncurses5 was sooo last year
                          (("ncursesw5") "ncursesw6"))
             (substitute* "Makefile"
                          (("/usr/local") (assoc-ref %outputs "out")))
             #t)))
       #:test-target "test"))
    (native-inputs
     `(("gettext" ,gnu-gettext)
       ("perl" ,perl) ; for the documentation
       ("pkg-config" ,pkg-config)
       ("ruby" ,ruby))) ; for tests
    (inputs
     `(("curl" ,curl)
       ("json-c" ,json-c)
       ("ncurses" ,ncurses)
       ("stfl" ,stfl)
       ("sqlite" ,sqlite)
       ("libxml2" ,libxml2)))
    (home-page "http://newsbeuter.org/")
    (synopsis "Text mode rss feed reader")
    (description "Newsbeuter is an innovative RSS feed reader for the text
console.  It supports OPML import/exports, HTML rendering, podcast (podbeuter),
offline reading, searching and storing articles to your filesystem, and many
more features.  Its user interface is coherent, easy to use, and might look
common to users of mutt and slrn.")
    (license license:x11)))
