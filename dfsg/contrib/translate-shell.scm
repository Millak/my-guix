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

(define-module (dfsg contrib translate-shell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages linux) ; util-linux
  #:use-module (gnu packages readline))

(define-public translate-shell
  (package
    (name "translate-shell")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/soimort/" name "/archive/v"
                            version ".tar.gz"))
        (sha256
         (base32
          "0akinlxbn179z1l6d4szc3wdvyil1i9y8jwmkna8fa0bigs89gbl"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure phase
         (add-after 'install 'emacs-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (dest  (string-append out "/share/emacs/site-lisp"))
                    (emacs (string-append (assoc-ref inputs "emacs") "/bin/emacs")))
               (install-file "google-translate-mode.el" dest)
               (emacs-generate-autoloads ,name dest)))))
       #:make-flags (list (string-append "PREFIX=" %output))
       #:imported-modules (,@%gnu-build-system-modules (guix build emacs-utils))
       #:modules ((guix build gnu-build-system)
                  (guix build emacs-utils)
                  (guix build utils))
       #:test-target "test"))
    (propagated-inputs
     `(("curl" ,curl)
       ("fribidi" ,fribidi)
       ("rlwrap" ,rlwrap)))
    (native-inputs
     `(("emacs" ,emacs-minimal)
       ("util-linux" ,util-linux))) ; hexdump, for the test
    (home-page "https://www.soimort.org/translate-shell")
    (synopsis "Translations from the command line")
    (description
     "Translate Shell (formerly Google Translate CLI) is a command-line
translator powered by Google Translate (default), Bing Translator,
Yandex.Translate and Apertium.  It gives you easy access to one of these
translation engines from your terminal.")
    (license license:public-domain)))
