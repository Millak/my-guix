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

(define-module (dfsg main global)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages code)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public global-with-tags
  ;; There seems to be some namespace collision
  (let ((global (@ (gnu packages code) global)))
    (package
      (inherit global)
      (name "global-with-tags")
      (arguments
       (substitute-keyword-arguments (package-arguments global)
         ((#:configure-flags flags)
          `(cons* (string-append "--with-universal-ctags="
                                 (assoc-ref %build-inputs "ctags") "/bin/ctags")
                  (string-append "--sysconfdir="
                                 (assoc-ref %outputs "out") "/share/gtags")
                  "--localstatedir=/var"
                  ,flags))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'post-install 'install-plugins
               (lambda _
                 (with-directory-excursion "plugin-factory"
                   (invoke "make" "install"))))
             (add-before 'install 'dont-install-to-/var
               (lambda _
                 (substitute* "gozilla/Makefile"
                   (("DESTDIR\\)\\$\\{localstatedir\\}") "TMPDIR)"))
                 #t))
             (add-after 'install-plugins 'wrap-program
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (wrap-program
                   (string-append (assoc-ref outputs "out")
                                  "/share/gtags/script/pygments_parser.py")
                   `("PYTHONPATH" ":" prefix (,(or (getenv "PYTHONPATH")
                                                   (getenv "GUIX_PYTHONPATH")))))
                 #t))))))
      (inputs
       `(("bash" ,bash-minimal)         ; for wrap-program
         ("ctags" ,universal-ctags)
         ("python" ,python-wrapper)
         ("python-pygments" ,python-pygments)
         ,@(package-inputs global))))))
