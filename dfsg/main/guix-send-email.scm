;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main guix-send-email)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages version-control))

(define-public guix-send-email
  (package
    (name "guix-send-email")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
       #:modules '((guix build utils))
       #:builder
       #~(begin
           (use-modules (guix build utils))
           (let ((dest (string-append #$output
                                      "/share/guix/extensions/send-email.scm"))
                 (git            #$(this-package-input "git"))
                 (git-send-email (assoc-ref %build-inputs "git:send-email")))
             (mkdir-p (dirname dest))
             (with-output-to-file dest
               (lambda ()
                 (format #t
"(define-module (guix extensions send-email)~@
                #:use-module (guix scripts)~@
                #:export (guix-send-email))~@
~@
(define-command (guix-send-email . args)~@
                (category extension)~@
                (synopsis \"Replace 'guix send-email' with 'git send-email'\")~@
                (setenv \"GIT_EXEC_PATH\" \"~a/libexec/git-core\")~@
                (apply system* \"~a/bin/git\" \"send-email\" args))~%"
                 git-send-email git)))))))
    (home-page "")  ; Should be documentation location for GUIX_EXTENSIONS_PATH
    (inputs
     `(("git" ,git)
       ("git:send-email" ,git "send-email")))
    (synopsis "Replace @code{guix send-email} with @code{git send-email}")
    (description "This Guix extension provides a shell redirect from @code{guix
send-email} to @code{git send-email}.")
    ;; The package definition is longer than the code;
    ;; let this serve as the declaration of the license.
    (license license:gpl3+)))
