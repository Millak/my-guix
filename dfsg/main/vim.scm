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

(define-module (dfsg main vim)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages web-browsers))

(define-public gvlc
  (let ((commit "456446e7fa898fed82fef0d0c2dd0adf9b4b3b89")
        (revision "1"))
    (package
      (name "gvlc")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/wafelack/gvlc")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "15y6kmlvrxncxv7p8y4yhd2w10ir9jsqnxrbli330jy85iqrlkgv"))))
      (build-system cargo-build-system)
      (arguments
       `(#:install-source? #f
         #:cargo-inputs
         (("rust-clap" ,rust-clap-2))))
      (home-page "https://github.com/wafelack/gvlc")
      (synopsis "Gentle Vim Lisp Compiler")
      (description "The Gentle Vim Lisp Compiler is a compiler for the Vim Lisp
programming language.")
      (license license:gpl3+))))

(define-public vim-commentary
  ;; Last tagged release was 2016.
  (let ((commit "349340debb34f6302931f0eb7139b2c11dfdf427")
        (revision "1"))
    (package
      (name "vim-commentary")
      (version (git-version "1.3" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/tpope/vim-commentary")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "01lpfcn2hmvxddcf97f4qx5vksxj1hwrxb0c8ri59z9lb9z2hgjd"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("doc" "share/vim/vimfiles/")
           ("plugin" "share/vim/vimfiles/"))))
      (home-page "https://www.vim.org/scripts/script.php?script_id=3695")
      (synopsis "Vim plugin to toggle comments in code")
      (description "This vim plugin helps with quickly commenting and
uncommenting sections of text.")
      (license license:vim))))

(define-public vim-gemivim
  ;; No releases have been tagged.
  (let ((commit "a6a090918733cd6b58f360ad63c599f8e3f9727b")
        (revision "1"))
    (package
      (name "vim-gemivim")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.sr.ht/~k1nkreet/gemivim")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1jc9i1zl98j856v8dj4pqbfymx7rjkqzk68hzzfn71rjwlzxf7gy"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("autoload" "share/vim/vimfiles/")
           ("plugin" "share/vim/vimfiles/"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'hardcode-gmni
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "autoload/gemivim.vim"
                 (("'gmni")
                  (string-append "'" (assoc-ref inputs "gmni") "/bin/gmni")))
               #t)))))
      (inputs
       `(("gmni" ,gmni)))
      (home-page "https://sr.ht/~k1nkreet/gemivim")
      (synopsis "VIM plugin for browsing Gemini pages")
      (description "This is a simple VIM plugin for browsing Gemini pages.
It uses @code{gmni} as a gemini client and supports:
@itemize
@item Get gemini content by given URL
@item Netrw gx-like open gemini link under cursor
@item Capturing input when gemini resource anticipates to
@item Bookmarks management
@end itemize
Since plugin does nothing but storing content in temporary files and opening
them in buffers, one could find quite convenient to use whole power of VIM with
search, jumplists, tabs, whatever to browse efficiently.")
      (license license:gpl2))))

(define-public vim-gemini-vim-syntax
  ;; No releases have been tagged.
  (let ((commit "596d1f36b386e5b2cc1af4f2f8285134626878d1")
        (revision "1"))
    (package
      (name "vim-gemini-vim-syntax")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://tildegit.org/sloum/gemini-vim-syntax")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1h5cq9fbv53nsjj66xrqmdiv0i1gqcvd0mfks3sdlnj0lzhbpip0"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("ftdetect" "share/vim/vimfiles/")
           ("syntax" "share/vim/vimfiles/"))))
      (home-page "https://tildegit.org/sloum/gemini-vim-syntax")
      (synopsis "Vim syntax highlighting for text/gemini files")
      (description "This Vim plugin provides a basic syntax highlighting for the
Gemini protocol's text/gemini format.")
      (license license:unlicense))))
