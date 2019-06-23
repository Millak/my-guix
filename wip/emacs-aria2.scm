;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip emacs-aria2)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs))

(define-public emacs-aria2
  (package
    (name "emacs-aria2")
    (version "0.20141108")
    (source
      (origin
        (method url-fetch)
        (uri "https://melpa.org/packages/aria2-20141107.1517.el")
        (sha256
         (base32
          "00slyi0aw03cp5s4c1xhavn9i1gz7d6d4cmv0g0dd3nx7m1ba3y0"))))
    (build-system emacs-build-system)
    (home-page "https://gitlab.com/ukaszg/aria2.git")
    (synopsis "Major mode for controlling aria2")
    (description "This is aria2, a major mode for controlling aria2c downloader

Currrently supported download types are: bittorrent, magnet, meta4, ftp, http,
https files (basically what aria2c supports).  There is no support for changing
global or per-download options, but this is planned.

This mode tries to work well with evil-mode, just set aria2-add-evil-quirks to t.")
    (license (license:non-copyleft
               "https://www.gnu.org/licenses/license-list.html#informal"))))

(define-public emacs-aria2el
  (let ((commit "ab1014b1063d95c3ba6e856877aa88573f2af904")
        (revision "1"))
    (package
      (inherit emacs-aria2)
      (name "emacs-aria2el")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/LdBeth/aria2.el.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1i4iggk8n56ln0blrilpbwp95ayr11ayxyxypna0nx2yyv86pj50"))))
      (home-page "https://github.com/LdBeth/aria2.el.git"))))
