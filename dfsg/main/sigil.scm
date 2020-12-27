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

(define-module (dfsg main sigil)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system minify)
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
    (version "1.4.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Sigil-Ebook/Sigil")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0jf0lphxks8dcf8qrm4dr1l635mvvs508rrhpsdh6pfvk15ijjgg"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (with-directory-excursion "3rdparty"
              (for-each delete-file-recursively
                        '("extra" "hunspell" "minizip" "pcre" "zlib")))
            (delete-file-recursively "src/Resource_Files/dictionaries")
            ;(delete-file-recursively "src/Resource_Files/polyfills/")   ; mathjax
            ;(delete-file "src/Resource_Files/polyfills/ML.zip")
            (with-directory-excursion "src/Resource_Files/javascript"
              (delete-file "jquery-2.2.4.min.js")
              (delete-file "jquery.scrollTo-1.4.2-min.js")
              (delete-file "jquery.scrollTo-2.1.2-min.js"))
            #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f  ; no tests
       #:configure-flags
       (list "-DUSE_SYSTEM_LIBS=1"
             "-DSYSTEM_LIBS_REQUIRED=1"
             "-DINSTALL_BUNDLED_DICTS=0"
             ;; Tries to install file to mathjax/config/local
             ;(string-append "-DMATHJAX_DIR="
             ;               (assoc-ref %build-inputs "js-mathjax"))
             "-DINSTALL_HICOLOR_ICONS=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-javascript-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (symlink (assoc-ref inputs "jquery-2.2.4.min.js")
                      "src/Resource_Files/javascript/jquery-2.2.4.min.js")
             (symlink (string-append (assoc-ref inputs "js-jquery-scrollto")
                                     "/share/javascript/jquery.scrollTo.min.js")
                      "src/Resource_Files/javascript/jquery.scrollTo-2.1.2-min.js")
             #t))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (qtwebengine (assoc-ref inputs "qtwebengine")))
               (wrap-program (string-append out "/bin/sigil")
                 `("QTWEBENGINEPROCESS_PATH" =
                   ,(list (string-append
                            qtwebengine
                            "/lib/qt5/libexec/QtWebEngineProcess")))
                 `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH"))))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)

       ("jquery-2.2.4.min.js"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://code.jquery.com/jquery-2.2.4.min.js"))
           (sha256
            (base32
             "13jglpbvm4cjqpbi82fsq8bi0b0ynwxd1nh8yvc19zqzyjb5vf05"))))))
    (inputs
     `(("hunspell" ,hunspell)
       ("js-mathjax" ,js-mathjax)
       ("js-jquery-scrollto" ,js-jquery-scrollto)
       ("minizip" ,minizip)
       ("pcre" ,pcre)
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
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)))
    (home-page "https://sigil-ebook.com/")
    (synopsis "EPUB ebook editor")
    (description "Sigil is an ebook editor that uses Qt.  It is designed to edit
books in ePub format (both ePub 2 and ePub 3).")
    (native-search-paths
      (list (search-path-specification
              (variable "SIGIL_DICTIONARIES")
              (files '("share/hunspell")))))
    (license license:gpl3+)))

(define-public js-jquery-scrollto
  (package
    (name "js-jquery-scrollto")
    (version "2.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/flesler/jquery.scrollTo")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0viaazjcxpk15bhnrrp08r8nd0gicpbc128mmcc0c31rkwc61a4l"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file "jquery.scrollTo.min.js") #t))))
    (build-system minify-build-system)
    (arguments `(#:javascript-files '("jquery.scrollTo.js")))
    (home-page "http://demos.flesler.com/jquery/scrollTo/")
    (synopsis "Customizable animated scrolling with jQuery")
    (description "@code{jQuery.ScrollTo} is a plug-in that lets you easily
scroll the page wherever you want with some nice effects.")
    (license license:expat)))
