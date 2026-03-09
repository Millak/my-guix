;;; Copyright © 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main zulip)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  )

(define-public zulip-terminal
  ;; Version 0.7.0 is from 2022.
  (let ((commit "6a799870eccc00d612e25ff881d18f4ff66d92fa")
        (revision "1"))
    (package
      (name "zulip-terminal")
      (version (git-version "0.7.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/zulip/zulip-terminal")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0sxf07fc9rarxcjl1mi8r5asnh9svggc7hfg9r425rh9qxnsda5i"))))
      (build-system pyproject-build-system)
      (arguments
       (list
         #:tests? #f        ; TODO: Skip the tests for now.
         #:test-flags
         #~(list "-k"
                 (string-join
                   (list "not test_main_multiple_autohide_options[options0]"
                         "test_main_multiple_autohide_options[options1]"
                         "TestMessageBox.test_soup2markup[link_userupload]"
                         "TestMessageBox.test_soup2markup[link_api]"
                         "TestMessageBox.test_soup2markup[preview-twitter]")
                   " and not "))
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'adjust-dependencies
               (lambda _
                 (substitute* "setup.py"
                   (("zulip>=0.8.2,<0.9.0") "zulip>=0.8.2")
                   (("urwid_readline>=0.15.1") "urwid_readline>=0.15.0")
                   (("lxml==4\\.9\\.4") "lxml>=4.9.4")
                   (("pygments>=2\\.17\\.2,<2\\.18\\.0") "pygments>=2.17.2,<3")
                   (("typing_extensions~=4\\.5\\.0") "typing_extensions>=4.5,<5")
                   (("tzlocal>=5\\.0,<5\\.1") "tzlocal>=5.0,<6"))))
             #;
             (add-after 'unpack 'patch-references
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "zulipterminal/platform_code.py"
                   (("notify-send")
                    (search-input-file inputs "bin/notify-send")))))
             )))
      (inputs
       (list libnotify
             python-urwid-for-zulip-term
             python-zulip
             python-urwid-readline
             python-beautifulsoup4
             python-lxml
             python-pygments
             python-typing-extensions
             python-dateutil
             python-pytz
             python-tzlocal
             python-pyperclip))
      (native-inputs
       (list python-pytest
             python-pytest-cov
             python-pytest-mock
             python-setuptools))
      (home-page "https://github.com/zulip/zulip-terminal")
      (synopsis "Zulip's official terminal client")
      (description "Zulip Terminal is the official terminal client for Zulip,
providing a @acronym{text-based user interface, TUI}.")
      (license license:asl2.0))))
