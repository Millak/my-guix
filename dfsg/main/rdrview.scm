;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main rdrview)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xml))

(define-public rdrview
  (let ((commit "00207dc516b035ae7760d005bd559077cc18e0b2")
        (revision "0"))
    (package
      (name "rdrview")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/eafer/rdrview")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0nkcsyl923zzrby8rdgsh97nzp7kg70j063frxh6bh3ikjd5sjgq"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f              ; Test suite needs work.
         #:make-flags (list (string-append "CC=" ,(cc-for-target))
                            (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)    ; no configure script.
           (add-after 'unpack 'patch-source
             ;; See https://github.com/eafer/rdrview/pull/2
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "Makefile"
                   (("/usr") "${PREFIX}")
                   (("^BINDIR") "PREFIX = /usr/local\nBINDIR")
                   (("^clean") "check:\n\tcd tests && ./check\n\nclean")))
               #t))
           (add-before 'check 'pre-check
             (lambda _
               (setenv "HOME" (getcwd))
               (with-output-to-file ".mailcap"
                 (lambda ()
                   (display "text/html;      links -dump %s; copiousoutput\n")))
               #t))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                 (with-directory-excursion "tests"
                   (invoke "./check" "-V"))
                 #t))))))
      (native-inputs
       `(("links" ,(@ (gnu packages web-browsers) links))
         ("tidy" ,(@ (gnu packages web) tidy))
         ("valgrind" ,(@ (gnu packages valgrind) valgrind))))
      (inputs
       `(("curl" ,curl)
         ("libseccomp" ,libseccomp)
         ("libxml2" ,libxml2)))
      (home-page "https://github.com/eafer/rdrview")
      (synopsis "Extract the main content from a webpage")
      (description "Command line tool to extract the main content from a
webpage, as done by the \"Reader View\" feature of most modern browsers.  It's
intended to be used with terminal RSS readers, to make the articles more
readable on web browsers such as @code{lynx}.  The code is closely adapted from
the @curl{https://github.com/mozilla/readability, Firefox version} and the
output is expected to be mostly equivalent.")
      (license license:asl2.0))))
