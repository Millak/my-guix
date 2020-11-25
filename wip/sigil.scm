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

(define-module (wip sigil)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml))

(define-public sigil
  (package
    (name "sigil")
    (version "1.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Sigil-Ebook/Sigil")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "14k4h6ncq9bj8hmnm0zpz0w0hffrcwbhc0bmparp6c6py2l66v26"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (with-directory-excursion "3rdparty"
              (for-each delete-file-recursively
                        '("extra" "hunspell" "minizip" "pcre" "zlib")))
            (delete-file-recursively "src/Resource_Files/dictionaries")
            (delete-file-recursively "src/Resource_Files/polyfills/")   ; mathjax
            ;(delete-file-recursively "src/Resource_Files/javascript/jquery-2.2.4.min.js")
            ;(delete-file-recursively "src/Resource_Files/javascript/jquery.scrollTo-1.4.2-min.js")
            ;(delete-file-recursively "src/Resource_Files/javascript/jquery.scrollTo-2.1.2-min.js")
            ;; supposedly modified versions of gumbo and beautifulsoup4
            ;(delete-file-recursively "internal")    ; gumbo
            ;(delete-file-recursively "src/Resource_Files/plugin_launchers/sigil_bs4")
            #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DUSE_SYSTEM_LIBS=ON"
             "-DSYSTEM_LIBS_REQUIRED=ON"
             "-DINSTALL_BUNDLED_DICTS=OFF"
             (string-append "-DMATHJAX_DIR="
                            (assoc-ref %build-inputs "js-mathjax"))
             "-DINSTALL_HICOLOR_ICONS=ON")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-chardet" ,python-chardet)
       ("python-css-parser" ,python-css-parser)
       ("python-cssselect" ,python-cssselect)
       ("python-dulwich" ,python-dulwich)
       ("python-html5lib" ,python-html5lib)
       ("python-lxml" ,python-lxml)
       ("python-pillow" ,python-pillow)
       ("python-pyqt" ,python-pyqt)
       ("python-regex" ,python-regex)
       ("python-six" ,python-six)
       ;("python-tk" ,python-tk)
       ("qttools" ,qttools)))
    (inputs
     `(("hunspell" ,hunspell)
       ("js-mathjax" ,js-mathjax)
       ("minizip" ,minizip)
       ("pcre" ,pcre)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebengine" ,qtwebengine)
       ("zlib" ,zlib)))
    (home-page "https://sigil-ebook.com/")
    (synopsis "EPUB ebook editor")
    (description "Sigil is an ebook editor that uses Qt.  It is designed to edit
books in ePub format (both ePub 2 and ePub 3).")
    ;; Borrow from hunspell
    (native-search-paths
      (list (search-path-specification
              (variable "DICPATH")
              (files '("share/hunspell")))))
    (license license:gpl3+)))
