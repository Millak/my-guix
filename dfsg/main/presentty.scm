;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main presentty)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages games)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public presentty
  (package
    (name "presentty")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "presentty" version))
        (sha256
         (base32
          "1qpy992hyg1amjl0acic3agj20spcpv5m0ncg1283mmxs8cs3xy9"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-version-requirements
           (lambda _
             (substitute* "requirements.txt"
               ((">.*") "\n"))
             #t))
         (replace 'wrap
           (lambda* (#:key python inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (python (assoc-ref inputs "python")))
               (for-each
                 (lambda (program)
                   (wrap-program (string-append bin program)
                     `("PATH" ":" prefix (,(dirname (which "cowsay"))
                                          ,(dirname (which "figlet"))
                                          ,(dirname (which "jp2a"))))
                     `("PYTHONPATH" prefix
                       ,(cons (string-append out "/lib/python"
                                            (python-version python)
                                            "/site-packages")
                              (search-path-as-string->list
                                (or (getenv "PYTHONPATH") ""))))))
                 '("presentty" "presentty-console")))
             #t)))))
    (inputs
     `(("cowsay" ,cowsay)
       ("figlet" ,figlet)
       ("jp2a" ,jp2a)
       ("python2-pillow" ,python2-pillow)
       ("python2-pygments" ,python2-pygments)
       ("python2-urwid" ,python2-urwid)))
    (native-inputs
     `(("python2-docutils" ,python2-docutils)
       ("python2-pbr" ,python2-pbr)))
    (home-page "http://git.inaugust.com/cgit/presentty/")
    (synopsis "Console-based presentation system")
    (description "Presentty is a console-based presentation program where slides
are authored in reStructuredText.  Its features include, but are not limited to:
Cross-fade animations, progressive list display, panning transitions, syntax
highlighting, Cowsay and figlet integration, ANSI art, JPEG display.")
    (license license:gpl3+)))
