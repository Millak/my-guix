;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip scsh)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages scheme))

(define-public scsh
  (package
    (name "scsh")
    (version "0.6.7")
    (source
      (origin
        (method url-fetch)
        (uri (list (string-append "https://ftp.scsh.net/pub/scsh/"
                                  (version-major+minor version)
                                  "/scsh-" version ".tar.gz")
                   (string-append "mirror://sourceforge/scsh/scsh-"
                                  version ".tar.gz")))
        (sha256
         (base32
          "0zyv3lnigpl950cz0m9ssd56smlsksm1ijdg7nmagdqb5bgzgaf4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-scheme48")))
    (inputs `(("scheme48" ,scheme48)))
    (home-page "https://scsh.net/")
    (synopsis "Scsh is an Unix shell embedded within Scheme")
    (description "Scsh has a high-level process notation for doing shell-script
like tasks: running programs, establishing pipelines and I/O redirection.  Scsh
embeds this process notation within a full implementation of Scheme, a minimal
and clean dialect of the Lisp programming language.  The process notation is
realized as a set of macro definitions, and is carefully designed to allow full
integration with standard Scheme code.  Scsh isn't Scheme-like; it is Scheme.
.
At the scripting level, scsh also has an Awk design, also implemented as a
macro that can be embedded inside general Scheme code.
.
Scsh additionally provides the low-level access to the operating system
normally associated with C.  The current release provides full access to POSIX,
plus important non-POSIX extensions, such as complete sockets support. \"Full
access to POSIX\" means: fork, exec & wait, sockets, full read, write, open &
close, seek & tell, complete file-system access, including stat,
chmod/chgrp/chown, symlink, FIFO & directory access, tty & pty support, file
locking, pipes, select, file-name pattern-matching, time & date, environment
variables, signal handlers, and more.")
    (license license:bsd-3)))
