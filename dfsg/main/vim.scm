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
  #:use-module (guix build-system copy))

(define-public vim-asyncrun
  (package
    (name "vim-asyncrun")
    (version "2.4.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/skywind3000/asyncrun.vim")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z64pkf37h3daqkry54ci3ars9wkrjskblgfpn29cwgmqg55n76n"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("plugin" "share/vim/vimfiles/")
         ("doc/" "share/vim/vimfiles/doc" #:include ("asyncrun.txt")))))
    (home-page "https://github.com/skywind3000/asyncrun.vim")
    (synopsis "Run Async Shell Commands in Vim")
    (description "This plugin takes the advantage of new APIs in Vim 8 (and
NeoVim) to enable you to run shell commands in background and read output in the
quickfix window in realtime.")
    (license license:expat)))
