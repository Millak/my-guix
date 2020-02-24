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

(define-module (dfsg main vim)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public vim-asyncrun
  (package
    (name "vim-asyncrun")
    (version "2.4.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/skywind3000/asyncrun.vim")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g82zk7fm8dhpac81xdbwmr33zsc8b1g4ip3j5xgvwrp28lw2wp3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vimfiles (string-append out "/share/vim/vimfiles")))
               (install-file "plugin/asyncrun.vim"
                             (string-append vimfiles "/plugin"))
               (install-file "doc/asyncrun.txt"
                             (string-append vimfiles "/doc"))
               #t))))))
    (home-page "https://github.com/skywind3000/asyncrun.vim")
    (synopsis "Run Async Shell Commands in Vim")
    (description "This plugin takes the advantage of new APIs in Vim 8 (and
NeoVim) to enable you to run shell commands in background and read output in the
quickfix window in realtime.")
    (license license:expat)))
