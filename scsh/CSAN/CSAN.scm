;;; CSAN.scm - Scheme Perl Archive Network
;;;
;;; Copyright (c) 2012 Johan Ceuppens
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; FXIME refactor

(define (CSAN-question~ droptext question answer defaultchoice procedure)
  (let ((s ""))
    (display droptext)
    (newline)
    (display question)(display " ")
    (display "[")(display defaultchoice)(display "] ")
    (set! s (read))
    (cond ((and (symbol? s)
                (string=? (symbol->string s) defaultchoice))
           (set! answer defaultchoice))
          ((and (number? s)
                (string=? (symbol->number s) defaultchoice))
           (set! answer defaultchoice))
          ((string=? (symbol->string s)(string #\return))
           (set! answer defaultchoice))
          ((and (symbol? s)(string? (symbol->string s)))
           (set! answer (symbol->string s)))
          ((and (number? s)(string? (number->string s)))
           (set! answer (number->string s)))
          (else (CSAN-question~ droptext question answer defaultchoice)))
    procedure))

(define CSAN-shell-droptext-1
  "1. The following questions are intended to help you with the
configuration. The CSAN module needs a directory of its own to cache
important index files and maybe keep a temporary mirror of CSAN files.
This may be a site-wide or a personal directory.")

(define CSAN-shell-droptext-2
  "2. Unless you are accessing the CSAN on your filesystem via a file: URL,
CSAN.pm needs to keep the source files it downloads somewhere. Please
supply a directory where the downloaded files are to be kept.")

(define CSAN-shell-droptext-3
  "3. ")

(define CSAN-shell-droptext-4
  "4. Normally CSAN.pm keeps config variables in memory and changes need to
be saved in a separate 'o conf commit' command to make them permanent
between sessions. If you set the 'auto_commit' option to true, changes
to a config variable are always automatically committed to disk.")

(define CSAN-shell-droptext-5
"5. CSAN.pm can limit the size of the disk area for keeping the build
directories with all the intermediate files.")

(define CSAN-shell-droptext-6
  "6. The CSAN indexes are usually rebuilt once or twice per hour, but the
typical CSAN mirror mirrors only once or twice per day. Depending on
the quality of your mirror and your desire to be on the bleeding edge,
you may want to set the following value to more or less than one day
(which is the default). It determines after how many days CSAN.pm
downloads new indexes.")

(define CSAN-shell-droptext-7
  "7. By default, each time the CSAN module is started, cache scanning is
performed to keep the cache size in sync. To prevent this, answer
'never'.")

(define CSAN-shell-droptext-8
  "8. To considerably speed up the initial CSAN shell startup, it is
possible to use Storable to create a cache of metadata. If Storable is
not available, the normal index mechanism will be used.

Note: this mechanism is not used when use_sqlite is on and SQLLite is
running.")

(define CSAN-shell-droptext-9
  "9. The CSAN module can detect when a module which you are trying to build
depends on prerequisites. If this happens, it can build the
prerequisites for you automatically ('follow'), ask you for
confirmation ('ask'), or just ignore them ('ignore'). Please set your
policy to one of the three values.")

;; (define CSAN-shell-droptext-10 ;;FIXME
;;   "Every Makefile.PL is run by perl in a separate process. Likewise we
;; run 'make' and 'make install' in separate processes. If you have
;; any parameters (e.g. PREFIX, UNINST or the like) you want to
;; pass to the calls, please specify them here.

;; If you don't understand this question, just press ENTER.

;; Typical frequently used settings:

;;     PREFIX=~/perl    # non-root users (please see manual for more hints)

;;  <makepl_arg>
;; Parameters for the 'perl Makefile.PL' command? [] ")


;; (define CSAN-shell-droptext-11 ;;FIXME
;; "Typical frequently used settings:

;;     PREFIX=~/perl    # non-root users (please see manual for more hints)

;;  <makepl_arg>
;; Parameters for the 'perl Makefile.PL' command? []

;; Parameters for the 'make' command? Typical frequently used setting:

;;     -j3              # dual processor system (on GNU make)

;;  <make_arg>
;; Your choice: [] ")


;; (define CSAN-shell-droptext-12 ;;FIXME
;; "Parameters for the 'make install' command?
;; Typical frequently used setting:

;;     UNINST=1         # to always uninstall potentially conflicting files

;;  <make_install_arg>
;; Your choice: []"

;; (define CSAN-shell-droptext-13 ;;FIXME
;; "A Build.PL is run by perl in a separate process. Likewise we run
;; './Build' and './Build install' in separate processes. If you have any
;; parameters you want to pass to the calls, please specify them here.

;; Typical frequently used settings:

;;     --install_base /home/xxx             # different installation directory

;;  <mbuildpl_arg>
;; Parameters for the 'perl Build.PL' command? [] ")

;; (define CSAN-shell-droptext-14 ;;FIXME
;; "Parameters for the './Build' command? Setting might be:

;;     --extra_linker_flags -L/usr/foo/lib  # non-standard library location

;;  <mbuild_arg>
;; Your choice: [] ")

;; (define CSAN-shell-droptext-15 ;;FIXME
;; "Do you want to use a different command for './Build install'? Sudo
;; users will probably prefer:

;;     su root -c ./Build
;;  or
;;     sudo ./Build
;;  or
;;     /path1/to/sudo -u admin_account ./Build

;;  <mbuild_install_build_command>")

;; (define CSAN-shell-droptext-16 ;;FIXME
;; "Parameters for the './Build install' command? Typical frequently used
;; setting:

;;     --uninst 1                           # uninstall conflicting files

;;  <mbuild_install_arg>
;; Your choice: [] ")

;; (define CSAN-shell-droptext-17 ;;FIXME
;; "If you're accessing the net via proxies, you can specify them in the
;; CSAN configuration or via environment variables. The variable in
;; the $CSAN::Config takes precedence.

;;  <ftp_proxy>
;; Your ftp_proxy? []")

;; (define CSAN-shell-droptext-18 ;;FIXME
;; " <http_proxy>
;; Your http_proxy? []")

;; (define CSAN-shell-droptext-19 ;;FIXME
;; "<no_proxy>
;; Your no_proxy? []")

(define CSAN-shell-droptext-20
"20. CSAN needs access to at least one CSAN mirror.

As you did not allow me to connect to the internet you need to supply
a valid CSAN URL now.
")

(define CSAN-shell-droptext-21
  "21. Enter another URL or RETURN to quit: []")

(define CSAN-shell-droptext-22
  "22. Please remember to call 'o conf commit' to make the config permanent!


cpan shell -- CSAN exploration and modules installation (v1.9402)
Enter 'h' for help.")

(define questions (make-table))
(table-set! questions 1 CSAN-shell-droptext-1)
(table-set! questions 2 CSAN-shell-droptext-2)
(table-set! questions 3 CSAN-shell-droptext-3)
(table-set! questions 4 CSAN-shell-droptext-4)
(table-set! questions 5 CSAN-shell-droptext-5)
(table-set! questions 6 CSAN-shell-droptext-6)
(table-set! questions 7 CSAN-shell-droptext-7)
(table-set! questions 8 CSAN-shell-droptext-8)
(table-set! questions 9 CSAN-shell-droptext-9)
(table-set! questions 20 CSAN-shell-droptext-20)
(table-set! questions 21 CSAN-shell-droptext-21)
(table-set! questions 22 CSAN-shell-droptext-22)

(define question?
  (lambda (n)
    (case ((n) (string-match (table-ref n) (rx (| ((string n)))))
               (table-ref n)))))

(define questionaire
  (lambda (CSAN-dir)
    ;; (fork-and-forget
    (define answer "")
    (define CSAN-build-and-cache-dir CSAN-dir)
    (define CSAN-download-target-dir CSAN-dir)
    (define CSAN-mirror-url (if (file-exists? (string-append CSAN-dir "/mirror"))

                                (read (string-append CSAN-dir "/mirror"))
                                "localhost"))
    ;; question 1
    ;;((lambda ()
       (cond
        ((and (file-exists? CSAN-build-and-cache-dir)
              (file-exists? (string-append CSAN-dir "/mirror")))
         (let ((CSAN-mirror-url (make-string-input-port
                                 (open-input-file (string-append CSAN-build-and-cache-dir "/mirror")))))
        (CSAN-shell-spawn CSAN-dir CSAN-mirror-url)))
     (else
;;;;;;;;prototype (define (CSAN-question~ droptext question answer defaultchoice)
      ((CSAN-question~ (question? 1)
                       "CSAN build and cache directory"
                       ""
                       CSAN-build-and-cache-dir
                       (lambda (answer)
                         (let ((dir (create-directory answer)))
                           (run (touch (string-append dir "/help")))
                           (let ((out (open-output-file (string-append dir "/help"))))
                             (write "Commands : 'h' and 'get <filename-on-server>"))
                           (if (file-directory? dir)
                               (set! CSAN-dir answer)
                               (set! CSAN-build-and-cache-dir answer)
                               #f)))) answer)

      (define CSAN-download-target-dir (string-append CSAN-build-and-cache-dir "/" "sources"))
      ((CSAN-question~ (question? 2)
                       "Download target directory"
                       ""
                       CSAN-download-target-dir
                       (lambda (answer)
                         (let ((dir (create-directory answer)))
                           (if (file-directory? answer)
                               (set! CSAN-download-target-dir answer)
                               #f)))) answer)

      (define CSAN-build-dir (string-append CSAN-build-and-cache-dir "/" "build"))
      ((CSAN-question~ (question? 3)
                       "Directory where the build process takes place?"
                       ""
                       CSAN-download-target-dir
                       (lambda (answer)
                         (let ((dir (create-directory answer)))
                           (if (file-directory? answer)
                               (set! CSAN-build-dir answer)
                               #f)))) answer)

      (define CSAN-config "no")
      ((CSAN-question~ (question? 4)
                       "Always commit changes to config variables to disk?"
                       ""
                       CSAN-config
                       (lambda (answer)
                         (set! CSAN-config answer)
                         #f)) answer)

      (define CSAN-build-Mb 100)
      ((CSAN-question~ (question? 5)
                       "Cache size for build directory (in MB)?"
                       ""
                       CSAN-build-Mb
                       (lambda (answer)
                         (set! CSAN-build-Mb answer)
                         #f)) answer)

      (define CSAN-expire 1)
      ((CSAN-question~ (question? 6)
                       "Let the index expire after how many days?"
                       ""
                       CSAN-expire
                       (lambda (answer)
                         (set! CSAN-expire answer)
                         #f)) answer)

      (define CSAN-scan-cache "atstart")
      ((CSAN-question~ (question? 7)
                       "Perform cache scanning (atstart or never)?"
                       ""
                       CSAN-scan-cache
                       (lambda (answer)
                         (set! CSAN-scan-cache answer)
                         #f)) answer)

      (define CSAN-cache-metadata "yes")
      ((CSAN-question~ (question? 8)
                       "Cache metadata (yes/no)?"
                       ""
                       CSAN-cache-metadata
                       (lambda (answer)
                         (set! CSAN-cache-metadata answer)
                         #f)) answer)

      (define CSAN-policy-building "ask")
      ((CSAN-question~ (question? 9)
                       "Policy on building prerequisites (follow, ask or ignore)? [ask]"
                       ""
                       CSAN-policy-building
                       (lambda (answer)
                         (set! CSAN-policy-building answer)
                         #f)) answer)

      ;; question 10 is under dev
      ;; question 11 is under dev
      ;; question 12 is under dev
      ;; question 13 is under dev

      ;; ... until 20


      ((CSAN-question~ (question? 20)
                       "Please enter the URL of your CSAN mirror "
                       ""
                       CSAN-mirror-url
                       (lambda (answer)
                         (set! CSAN-mirror-url answer)
                         (run (touch (string-append CSAN-dir "/mirror")))
                         (let ((out (open-output-file (string-append CSAN-dir "/mirror"))))
                           (write answer out))
                         #f)) answer)

      (define CSAN-mirror-url-2 "")
      ((CSAN-question~ (question? 21)
                       "Enter another URL or RETURN to quit: [] "
                       ""
                       CSAN-mirror-url-2
                       (lambda (answer)
                         (set! CSAN-mirror-url-2 answer)
                         #f)) answer)

      (display (question? 22))
      (CSAN-shell-spawn CSAN-dir CSAN-mirror-url))
     )))

