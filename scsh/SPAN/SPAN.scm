;;; SPAN.scm - Scheme Perl Archive Network
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

(define (SPAN-question~ droptext question answer defaultchoice procedure)
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
          (else (SPAN-question~ droptext question answer defaultchoice)))
    procedure))

(define SPAN-shell-droptext-1
  "1. The following questions are intended to help you with the
configuration. The SPAN module needs a directory of its own to cache
important index files and maybe keep a temporary mirror of SPAN files.
This may be a site-wide or a personal directory.")

(define SPAN-shell-droptext-2
  "2. Unless you are accessing the CPAN on your filesystem via a file: URL,
CPAN.pm needs to keep the source files it downloads somewhere. Please
supply a directory where the downloaded files are to be kept.")

(define SPAN-shell-droptext-3
  "3. ")

(define SPAN-shell-droptext-4
  "4. Normally CPAN.pm keeps config variables in memory and changes need to
be saved in a separate 'o conf commit' command to make them permanent
between sessions. If you set the 'auto_commit' option to true, changes
to a config variable are always automatically committed to disk.")

(define SPAN-shell-droptext-5
"5. CPAN.pm can limit the size of the disk area for keeping the build
directories with all the intermediate files.")

(define SPAN-shell-droptext-6
  "6. The CPAN indexes are usually rebuilt once or twice per hour, but the
typical CPAN mirror mirrors only once or twice per day. Depending on
the quality of your mirror and your desire to be on the bleeding edge,
you may want to set the following value to more or less than one day
(which is the default). It determines after how many days CPAN.pm
downloads new indexes.")

(define SPAN-shell-droptext-7
  "7. By default, each time the CPAN module is started, cache scanning is
performed to keep the cache size in sync. To prevent this, answer
'never'.")

(define SPAN-shell-droptext-8
  "8. To considerably speed up the initial CPAN shell startup, it is
possible to use Storable to create a cache of metadata. If Storable is
not available, the normal index mechanism will be used.

Note: this mechanism is not used when use_sqlite is on and SQLLite is
running.")

(define SPAN-shell-droptext-9
  "9. The CPAN module can detect when a module which you are trying to build
depends on prerequisites. If this happens, it can build the
prerequisites for you automatically ('follow'), ask you for
confirmation ('ask'), or just ignore them ('ignore'). Please set your
policy to one of the three values.")

;; (define SPAN-shell-droptext-10 ;;FIXME
;;   "Every Makefile.PL is run by perl in a separate process. Likewise we
;; run 'make' and 'make install' in separate processes. If you have
;; any parameters (e.g. PREFIX, UNINST or the like) you want to
;; pass to the calls, please specify them here.

;; If you don't understand this question, just press ENTER.

;; Typical frequently used settings:

;;     PREFIX=~/perl    # non-root users (please see manual for more hints)

;;  <makepl_arg>
;; Parameters for the 'perl Makefile.PL' command? [] ")


;; (define SPAN-shell-droptext-11 ;;FIXME
;; "Typical frequently used settings:

;;     PREFIX=~/perl    # non-root users (please see manual for more hints)

;;  <makepl_arg>
;; Parameters for the 'perl Makefile.PL' command? []

;; Parameters for the 'make' command? Typical frequently used setting:

;;     -j3              # dual processor system (on GNU make)

;;  <make_arg>
;; Your choice: [] ")


;; (define SPAN-shell-droptext-12 ;;FIXME
;; "Parameters for the 'make install' command?
;; Typical frequently used setting:

;;     UNINST=1         # to always uninstall potentially conflicting files

;;  <make_install_arg>
;; Your choice: []"

;; (define SPAN-shell-droptext-13 ;;FIXME
;; "A Build.PL is run by perl in a separate process. Likewise we run
;; './Build' and './Build install' in separate processes. If you have any
;; parameters you want to pass to the calls, please specify them here.

;; Typical frequently used settings:

;;     --install_base /home/xxx             # different installation directory

;;  <mbuildpl_arg>
;; Parameters for the 'perl Build.PL' command? [] ")

;; (define SPAN-shell-droptext-14 ;;FIXME
;; "Parameters for the './Build' command? Setting might be:

;;     --extra_linker_flags -L/usr/foo/lib  # non-standard library location

;;  <mbuild_arg>
;; Your choice: [] ")

;; (define SPAN-shell-droptext-15 ;;FIXME
;; "Do you want to use a different command for './Build install'? Sudo
;; users will probably prefer:

;;     su root -c ./Build
;;  or
;;     sudo ./Build
;;  or
;;     /path1/to/sudo -u admin_account ./Build

;;  <mbuild_install_build_command>")

;; (define SPAN-shell-droptext-16 ;;FIXME
;; "Parameters for the './Build install' command? Typical frequently used
;; setting:

;;     --uninst 1                           # uninstall conflicting files

;;  <mbuild_install_arg>
;; Your choice: [] ")

;; (define SPAN-shell-droptext-17 ;;FIXME
;; "If you're accessing the net via proxies, you can specify them in the
;; CPAN configuration or via environment variables. The variable in
;; the $CPAN::Config takes precedence.

;;  <ftp_proxy>
;; Your ftp_proxy? []")

;; (define SPAN-shell-droptext-18 ;;FIXME
;; " <http_proxy>
;; Your http_proxy? []")

;; (define SPAN-shell-droptext-19 ;;FIXME
;; "<no_proxy>
;; Your no_proxy? []")

(define SPAN-shell-droptext-20
"20. CPAN needs access to at least one CPAN mirror.

As you did not allow me to connect to the internet you need to supply
a valid CPAN URL now.
")

(define SPAN-shell-droptext-21
  "21. Enter another URL or RETURN to quit: []")

(define SPAN-shell-droptext-22
  "22. Please remember to call 'o conf commit' to make the config permanent!


cpan shell -- CPAN exploration and modules installation (v1.9402)
Enter 'h' for help.")

(define questions (make-table))
(table-set! questions 1 SPAN-shell-droptext-1)
(table-set! questions 2 SPAN-shell-droptext-2)
(table-set! questions 3 SPAN-shell-droptext-3)
(table-set! questions 4 SPAN-shell-droptext-4)
(table-set! questions 5 SPAN-shell-droptext-5)
(table-set! questions 6 SPAN-shell-droptext-6)
(table-set! questions 7 SPAN-shell-droptext-7)
(table-set! questions 8 SPAN-shell-droptext-8)
(table-set! questions 9 SPAN-shell-droptext-9)
(table-set! questions 20 SPAN-shell-droptext-20)
(table-set! questions 21 SPAN-shell-droptext-21)
(table-set! questions 22 SPAN-shell-droptext-22)

(define table-range
  (lambda (x y)
    (let ((l '()))
      (cond ((< x y)
             (do ((i x (+ i 1)))
                 ((= x y) l)
               (set! l (append l (table-ref questions i)))))
            ((< y x)
             (do ((i y (+ i 1)))
                 ((= y x) l)
               (set! l (append l (table-ref questions i)))))
            (else (display-msg "range : x == y")
                  x)))))

(define question?
  (lambda (n)
    (case ((n) (string-match (table-ref n) (rx (| ((string n)))))
               (table-ref n)))))

(define questionaire
  (lambda (SPAN-dir)
    (define answer "")
    (define SPAN-build-and-cache-dir SPAN-dir)
    (define SPAN-download-target-dir SPAN-dir)
    (define SPAN-mirror-url "localhost")
    ;; question 1
    ;;((lambda ()
       (cond
        ((and (file-exists? SPAN-build-and-cache-dir)
              (file-exists? (string-append SPAN-dir "/mirror")))
         (let ((SPAN-mirror-url (make-string-input-port
                                 (open-input-file (string-append SPAN-build-and-cache-dir "/mirror")))))
        (SPAN-shell-spawn SPAN-mirror-url)))
     (else
;;;;;;;;prototype (define (SPAN-question~ droptext question answer defaultchoice)
      ((SPAN-question~ (question? 1)
                       "SPAN build and cache directory"
                       ""
                       SPAN-build-and-cache-dir
                       (lambda (answer)
                         (let ((dir (create-directory answer)))
                           (if (file-directory? dir)
                               (set! SPAN-dir answer)
                               (set! SPAN-build-and-cache-dir answer)
                               #f)))) answer)

      (define SPAN-download-target-dir (string-append SPAN-build-and-cache-dir "/" "sources"))
      ((SPAN-question~ (question? 2)
                       "Download target directory"
                       ""
                       SPAN-download-target-dir
                       (lambda (answer)
                         (let ((dir (create-directory answer)))
                           (if (file-directory? answer)
                               (set! SPAN-download-target-dir answer)
                               #f)))) answer)

      (define SPAN-build-dir (string-append SPAN-build-and-cache-dir "/" "build"))
      ((SPAN-question~ (question? 3)
                       "Directory where the build process takes place?"
                       ""
                       SPAN-download-target-dir
                       (lambda (answer)
                         (let ((dir (create-directory answer)))
                           (if (file-directory? answer)
                               (set! SPAN-build-dir answer)
                               #f)))) answer)

      (define SPAN-config "no")
      ((SPAN-question~ (question? 4)
                       "Always commit changes to config variables to disk?"
                       ""
                       SPAN-config
                       (lambda (answer)
                         (set! SPAN-config answer)
                         #f)) answer)

      (define SPAN-build-Mb 100)
      ((SPAN-question~ (question? 5)
                       "Cache size for build directory (in MB)?"
                       ""
                       SPAN-build-Mb
                       (lambda (answer)
                         (set! SPAN-build-Mb answer)
                         #f)) answer)

      (define SPAN-expire 1)
      ((SPAN-question~ (question? 6)
                       "Let the index expire after how many days?"
                       ""
                       SPAN-expire
                       (lambda (answer)
                         (set! SPAN-expire answer)
                         #f)) answer)

      (define SPAN-scan-cache "atstart")
      ((SPAN-question~ (question? 7)
                       "Perform cache scanning (atstart or never)?"
                       ""
                       SPAN-scan-cache
                       (lambda (answer)
                         (set! SPAN-scan-cache answer)
                         #f)) answer)

      (define SPAN-cache-metadata "yes")
      ((SPAN-question~ (question? 8)
                       "Cache metadata (yes/no)?"
                       ""
                       SPAN-cache-metadata
                       (lambda (answer)
                         (set! SPAN-cache-metadata answer)
                         #f)) answer)

      (define SPAN-policy-building "ask")
      ((SPAN-question~ (question? 9)
                       "Policy on building prerequisites (follow, ask or ignore)? [ask]"
                       ""
                       SPAN-policy-building
                       (lambda (answer)
                         (set! SPAN-policy-building answer)
                         #f)) answer)

      ;; question 10 is under dev
      ;; question 11 is under dev
      ;; question 12 is under dev
      ;; question 13 is under dev

      ;; ... until 20


      ((SPAN-question~ (question? 20)
                       "Please enter the URL of your CPAN mirror "
                       ""
                       SPAN-mirror-url
                       (lambda (answer)
                         (set! SPAN-mirror-url answer)
                         (let ((out (open-output-file (string-append SPAN-build-and-cache-dir "/mirror"))))
                           (write answer out))
                         #f)) answer)

      (define SPAN-mirror-url-2 "")
      ((SPAN-question~ (question? 21)
                       "Enter another URL or RETURN to quit: [] "
                       ""
                       SPAN-mirror-url-2
                       (lambda (answer)
                         (set! SPAN-mirror-url-2 answer)
                         #f)) answer)

      (display (question? 22))
      (SPAN-shell-spawn SPAN-mirror-url))
     )))

