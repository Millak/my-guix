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

(define-module (wip scroll)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages ncurses))

(define-public scroll
  (package
    (name "scroll")
    (version "1.20180421")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/scroll/scroll-"
               version ".tar.gz"))
        (sha256
         (base32
          "0apzrvf99rskj4dbmn57jjxrsf19j436s8a09m950df5aws3a0wj"))))
    (build-system haskell-build-system)
    (inputs
      `(
        ("ghc-case-insensitive" ,ghc-case-insensitive)
        ("ghc-data-default" ,ghc-data-default)
        ("ghc-ifelse" ,ghc-ifelse)
        ("ghc-monad-loops" ,ghc-monad-loops)
        ("ghc-ncurses" ,ghc-ncurses)
        ("ghc-optparse-applicative" ,ghc-optparse-applicative)
        ("ghc-random" ,ghc-random)
        ;("ghc-text" ,ghc-text)
        ("ghc-vector" ,ghc-vector)
        ))
    (home-page "https://joeyh.name/code/scroll/")
    (synopsis "scroll(6), a roguelike game")
    (description
     "You're a bookworm that's stuck on a scroll.  You have to dodge between
words and use spells to make your way down the page as the scroll is read.  Go
too slow and you'll get wound up in the scroll and crushed.")
    (license license:gpl2)))

(define-public ghc-ncurses
  (package
    (name "ghc-ncurses")
    (version "0.2.16")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/ncurses/ncurses-"
               version ".tar.gz"))
        (sha256
         (base32
          "0gsyyaqyh5r9zc0rhwpj5spyd6i4w2vj61h4nihgmmh0yyqvf3z5"))))
    (build-system haskell-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-includes
           (lambda _
             (substitute* '("cbits/hsncurses-shim.h"
                            "lib/UI/NCurses.chs"
                            "lib/UI/NCurses/Enums.chs")
               (("ncursesw/ncurses.h") "ncurses.h"))
             #t)))
       #:cabal-revision
       ("1"
        "1wfdy716s5p1sqp2gsg43x8wch2dxg0vmbbndlb2h3d8c9jzxnca")))
    (inputs `(("ncurses" ,ncurses5)))
    (native-inputs `(("ghc-c2hs" ,ghc-c2hs)))
    (home-page "https://john-millikin.com/software/haskell-ncurses/")
    (synopsis "Modernised bindings to GNU ncurses")
    (description "GNU ncurses is a library for creating command-line application
with pseudo-graphical interfaces.  This package is a nice, modern binding to GNU
ncurses.")
    (license license:gpl3)))

(define-public ncurses5
  (package
    (inherit ncurses)
    (name "ncurses5")
    (arguments
     (substitute-keyword-arguments (package-arguments ncurses)
       ((#:configure-flags cf)
        `(cons* "--with-abi-version=5"
                "--without-cxx-binding"
                "--without-normal"
                ,cf))))))
