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

(define-module (dfsg main dictionaries)
  #:use-module (guix gexp)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages qt))

(define-public qtwebengine-dictionary-en-us
  (computed-file "en-US.bdic"
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((dict (string-append #$output "/share/hunspell-bdic/en-US.bdic")))
            (mkdir-p (dirname dict))
            (invoke #+(file-append qtwebengine "/lib/qt6/libexec/qwebengine_convert_dict")
                    #$(file-append hunspell-dict-en-us "/share/hunspell/en_US.dic")
                    dict))))
    #:options '(#:target #f)))

(define-public qtwebengine-dictionary-he-il
  (computed-file "he-IL.bdic"
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((dict (string-append #$output "/share/hunspell-bdic/he-IL.bdic")))
            (mkdir-p (dirname dict))
            (invoke #+(file-append qtwebengine "/lib/qt6/libexec/qwebengine_convert_dict")
                    #$(file-append hunspell-dict-he-il "/share/hunspell/he_IL.dic")
                    dict))))
    #:options '(#:target #f)))
