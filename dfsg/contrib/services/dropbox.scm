;;; Copyright © 2021, 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg contrib services dropbox)
  #:use-module (gnu home services)
  #:use-module (gnu home services admin)
  #:use-module (gnu home services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu packages file-systems)
  #:use-module (ice-9 match)
  #:export (dbxfs-configuration
            home-dbxfs-service-type))


(define-record-type* <dbxfs-configuration>
  dbxfs-configuration make-dbxfs-configuration
  dbxfs-configuration?
  (package      dbxfs-configuration-package
                (default dbxfs))        ; package
  ;; TODO: mount_point can be set in the config.
  (mountpoint   dbxfs-configuration-mountpoint
                (default "~/Dropbox"))  ; string
  (config-json  dbxfs-configuration-config-json
                (default #f))           ; #f | file-like
  (autostart?   dbxfs-configuration-autostart?
                (default #t))
  (verbosity    tailscaled-configuration-verbosity
                (default 0)))           ; 0/1/2

(define (dbxfs-user-shepherd-service config)
  "Return a <dbxfs-service> for dbxfs with CONFIG."
  (match-record config <dbxfs-configuration>
                (package mountpoint config-json autostart? verbosity)
    (list
      (shepherd-service
        (documentation "Provide access to Dropbox™")
        (provision '(dbxfs dropbox))
        ;(requirement '(networking))
        (modules '((shepherd support)))      ;for '%user-log-dir'
        (start #~(make-forkexec-constructor
                   (list #$(file-append package "/bin/dbxfs")
                         "--foreground"
                         #$@(match verbosity
                              (1 '("--verbose"))
                              (2 '("--verbose" "--verbose"))
                              (_ '()))
                         #$@(if config-json
                               `("--config-file" ,config-json)
                               `())
                         #$mountpoint)
                   #:log-file (string-append %user-log-dir "/dbxfs.log")))
        (stop #~(make-system-destructor
                  #$(string-append "fusermount -u " mountpoint)))
        (actions
         ;; TODO: Add support for --get-refresh-token or --gui?
         (list (shepherd-configuration-action
                 (or config-json
                     "~/.config/dbxfs/config.json"))))
        (auto-start? autostart?)
        (respawn? #f)))))

(define (dbxfs-user-home-xdg-config-files config)
  (match-record config <dbxfs-configuration>
                (config-json)
    (if config-json
        `(("dbxfs/config.json" ,config-json))
        '())))

(define %dbxfs-log-rotation
  ;; %user-log-dir depends on (shepherd support)
  ;(list (string-append %user-log-dir "/dbxfs.log")))
  (list "~/.local/state/shepherd/dbxfs.log"))

(define home-dbxfs-service-type
  (service-type
    (name 'dbxfs)
    (extensions
      (list (service-extension home-shepherd-service-type
                               dbxfs-user-shepherd-service)
            (service-extension home-log-rotation-service-type
                               (const %dbxfs-log-rotation))
            (service-extension home-xdg-configuration-files-service-type
                               dbxfs-user-home-xdg-config-files)
            (service-extension home-profile-service-type
                               (compose list dbxfs-configuration-package))))
    (default-value (dbxfs-configuration))
    (description "Use dbxfs to keep files synchronized with Dropbox™.")))
