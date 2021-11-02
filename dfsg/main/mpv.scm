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
;;; You should have received a trivial of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (dfsg main mpv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages lua))

(define-public mpv-sponsorblock-minimal
  (let ((commit "9ae5f21658fbc0050e2acad179296da904dde0f3")     ; Sept 28, 2021
        (revision "4"))
  (package
    (name "mpv-sponsorblock-minimal")
    (version (git-version "0" revision commit))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://codeberg.org/jouni/mpv_sponsorblock_minimal")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0srxfldx9cra2n7rp01ji70z4rv8i7j3161gp8b3yx7ccp8w5qr1"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (lib    (string-append out "/lib"))
                (curl   (assoc-ref %build-inputs "curl"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/sponsorblock_minimal.lua")
                         lib)
           (install-file (string-append source "/LICENSE")
                         (string-append out "/share/doc/" ,name "-" ,version))
           (substitute* (string-append lib "/sponsorblock_minimal.lua")
             (("curl") (string-append curl "/bin/curl")))))))
    (inputs
     `(("curl" ,curl-minimal)))
    (home-page "https://codeberg.org/jouni/mpv_sponsorblock_minimal")
    (synopsis "Skips sponsored segments of YouTube videos")
    (description "This package provides a plugin to @code{mpv} to skip
sponsored segments of YouTube videos.")
    (license license:gpl3))))

(define-public mpv-twitch-chat
  (let ((commit "71dfa17b4fbbba88a81e13f8954353cdd5b7eddf")     ; Sept 4, 2021
        (revision "1"))
    (package
      (name "mpv-twitch-chat")
      (version (git-version "0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/CrendKing/mpv-twitch-chat")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0qajfvykc9jbbgf9v2684gihywfnnvpvzrdrhaabpcvnk27byb52"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out      (assoc-ref %outputs "out"))
                  (lib      (string-append out "/lib"))
                  (curl     (assoc-ref %build-inputs "curl"))
                  (lua-json (assoc-ref %build-inputs "lua-json"))
                  (source   (assoc-ref %build-inputs "source")))
             (install-file (string-append source "/main.lua") lib)
             (install-file (string-append source "/LICENSE")
                           (string-append out "/share/doc/" ,name "-" ,version))
             (substitute* (string-append lib "/main.lua")
               (("\"curl\"") (string-append "\"" curl "/bin/curl\""))
               (("^package\\.path.*") "")
               (("local json .*")
                (string-append "local json = loadfile(\"" lua-json
                               "/share/lua/5.2/json.lua\")()")))))))
      (inputs
       `(("curl" ,curl-minimal)
         ("lua-json" ,lua5.2-json)))
      (home-page "https://github.com/CrendKing/mpv-twitch-chat/")
      (synopsis "Twitch chat messages as subtitles")
      (description "Show Twitch chat messages as subtitles when watching Twitch
VOD with @code{mpv}.  @code{mpv} internally uses @code{youtube-dl} to handle
Twitch VOD URL.  In addition to the regular video track, it also adds a
\"rechat\" subtitle track.  This track points to the Twitch API
@code{videos/<video_id>/comments}, which contains the full transcript of a VOD's
chat messages in JSON.  Unfortunately, @code{mpv} can't directly consume the
JSON as subtitle.  This script converts it into a SubRip subtitle track so that
@code{mpv} can directly display the chat messages.")
      (license license:expat))))

(define (make-lua-json name lua)
  (package
    (name name)
    (version "0.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rxi/json.lua")
               (commit (string-append "v" version))))
        (file-name (git-file-name "lua-json" version))
        (sha256
         (base32 "0sgcf7x8nds3abn46p4fg687pk6nlvsi82x186vpaj2dbv28q8i5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "test"
                 (invoke "lua" "test.lua")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (lua-version ,(version-major+minor (package-version lua))))
               (install-file "json.lua"
                             (string-append out "/share/lua/" lua-version))
               #t))))))
    (inputs
     `(("lua" ,lua)))
    (home-page "https://github.com/rxi/json.lua")
    (synopsis "JSON library for Lua")
    (description "This package provides a lightweight JSON library for Lua.")
    (license license:expat)))

(define-public lua-json
  (make-lua-json "lua-json" lua))

(define-public lua5.1-json
  (make-lua-json "lua5.1-json" lua-5.1))

(define-public lua5.2-json
  (make-lua-json "lua5.2-json" lua-5.2))
