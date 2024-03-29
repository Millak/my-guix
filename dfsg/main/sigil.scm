;;; Copyright © 2020-2022 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix gexp)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system minify)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages hunspell)
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
    (version "1.9.30")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Sigil-Ebook/Sigil")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1byx8x4lx4lp8l1q6nmnx019zg5c9jvpf3sjmdwrcmngn7i9v8kx"))
        (snippet
         #~(begin
            (use-modules (guix build utils))
            (with-directory-excursion "3rdparty"
              (for-each delete-file-recursively
                        '("extra" "hunspell" "minizip" "pcre2" "zlib")))
            (delete-file-recursively "src/Resource_Files/dictionaries")
            ;(delete-file "src/Resource_Files/polyfills/custom-mathjax.min.js")
            (with-directory-excursion "src/Resource_Files/javascript"
              (delete-file "jquery-2.2.4.min.js")
              (delete-file "jquery.scrollTo-2.1.2-min.js"))))))
    (build-system qt-build-system)
    (arguments
     (list
       #:tests? #f  ; no tests
       #:configure-flags
       #~(list "-DUSE_SYSTEM_LIBS=1"
               "-DUSE_QT6=0"        ; For now.
               "-DUSE_NEWER_FINDPYTHON3=1"
               "-DSYSTEM_LIBS_REQUIRED=1"
               "-DINSTALL_BUNDLED_DICTS=0"
               "-DINSTALL_HICOLOR_ICONS=1"
               "-DDISABLE_UPDATE_CHECK=1"
               ;(string-append "-DMATHJAX3_DIR=" #$output
               ;               #$(this-package-input "js-mathjax-3")
               ;               "/share/javascript/mathjax")
               )
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'replace-javascript-libraries
             (lambda* (#:key inputs #:allow-other-keys)
               (symlink
                 (search-input-file inputs
                                    "/share/javascript/jquery.min.js")
                 "src/Resource_Files/javascript/jquery-2.2.4.min.js")
               (symlink
                 (search-input-file inputs
                                    "/share/javascript/jquery.scrollTo.min.js")
                 "src/Resource_Files/javascript/jquery.scrollTo-2.1.2-min.js")))
           (add-after 'unpack 'find-python3
             (lambda _
               (substitute* '("src/Dialogs/PluginRunner.cpp"
                              "src/Dialogs/PreferenceWidgets/PluginWidget.cpp")
                (("python3\\.4") "python3"))))
           (add-after 'install 'wrap-binary
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/sigil")
                   `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                   `("PATH" ":" prefix (,(which "python3")))))))
           (add-after 'install 'compile-python-bytecode
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (invoke "python3" "-m" "compileall"
                         (string-append out "/share/sigil/plugin_launchers/python/"))
                 (invoke "python3" "-m" "compileall"
                         (string-append out "/share/sigil/python3lib/"))))))))
    (native-inputs
     (list pkg-config
           qttools-5))
    ;(propagated-inputs
    ; ;; Needed so when sigil is installed the mathjax addon will be in the correct folder.
    ; ;; Needs to be mathjax 3.22+
    ; (list js-mathjax-3))
    (inputs
     (list bash-minimal     ; for wrap-program
           hunspell
           js-jquery
           js-jquery-scrollto
           minizip
           pcre2
           python
           (list python "tk")
           python-chardet
           python-css-parser
           python-cssselect
           python-dulwich
           python-html5lib
           python-lxml
           python-pillow
           python-pyqt
           python-pyqtwebengine
           python-regex
           python-six
           qtbase-5
           qtdeclarative-5
           qtwebchannel-5
           qtwebengine-5
           zlib))
    (home-page "https://sigil-ebook.com/")
    (synopsis "EPUB ebook editor")
    (description "Sigil is an ebook editor that uses Qt.  It is designed to edit
books in ePub format (both ePub 2 and ePub 3).")
    (native-search-paths
      (list (search-path-specification
              (variable "SIGIL_DICTIONARIES")
              (files '("share/hunspell")))))
    (license license:gpl3+)))

(define-public js-jquery
  (package
    (name "js-jquery")
    (version "2.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jquery/jquery")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "12x6k47kjbp3r8dipbjq70v32izxdakyr150pkxv5b29pp0p4sgx"))
        ;(snippet
        ; #~(begin
        ;    (use-modules (guix build utils))
        ;    (delete-file-recursively "dist")))
        ))
    (build-system minify-build-system)
    (arguments `(#:javascript-files '("dist/jquery.js")))
    (home-page "https://jquery.com/")
    (synopsis "jQuery JavaScript Library")
    (description "jQuery is a fast, small, and feature-rich JavaScript library.
It makes things like HTML document traversal and manipulation, event handling,
animation, and Ajax much simpler with an easy-to-use API that works across a
multitude of browsers.")
    (license license:expat)))

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
        (snippet
         #~(begin (delete-file "jquery.scrollTo.min.js")))))
    (build-system minify-build-system)
    (arguments `(#:javascript-files '("jquery.scrollTo.js")))
    (home-page "http://demos.flesler.com/jquery/scrollTo/")
    (synopsis "Customizable animated scrolling with jQuery")
    (description "@code{jQuery.ScrollTo} is a plug-in that lets you easily
scroll the page wherever you want with some nice effects.")
    (license license:expat)))
