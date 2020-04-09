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
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public vim-dispatch
  (package
    (name "vim-dispatch")
    (version "1.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tpope/vim-dispatch")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1m8b5mn2zqlphzs6xfwykwmghf6p0wabrhpjmh7vav35jgcxc4wl"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (home-page "https://github.com/tpope/vim-dispatch")
    (synopsis "Asynchronous build and test dispatcher")
    (description "Leverage the power of Vim's compiler plugins without being
bound by synchronicity.  Kick off builds and test suites using one of several
asynchronous adapters (including tmux, screen, and a headless mode), and when
the job completes, errors will be loaded and parsed automatically.")
    (license license:vim)))
