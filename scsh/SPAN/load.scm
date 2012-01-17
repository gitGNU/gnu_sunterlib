;;; schemedoc.scm - a scheme perldoc utility
;;;
;;; Copyright (c) 2011-2012 Johan Ceuppens
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

(load "SPAN.scm")

;; initialization

;; question 1

(define SPAN-build-and-cache-dir (string-append (getenv "HOME") "/.span"))
(define SPAN-download-target-dir (string-append (getenv "HOME") "/.span"))
;;prototype (define (SPAN-question~ droptext question answer defaultchoice)
(SPAN-question~ SPAN-shell-droptext-1
                "SPAN build and cache directory"
                ""
                SPAN-build-and-cache-dir
                (lambda (answer)
                  (let ((dir (create-directory answer)))
                    (if (file-directory? dir)
                        (set! SPAN-build-and-cache-dir answer)
                        #f))))

(define SPAN-download-target-dir (string-append SPAN-build-and-cache-dir "/" "sources")
(SPAN-question~ SPAN-shell-droptext-2
                "Download target directory"
                ""
                SPAN-download-target-dir
                (lambda (answer)
                  (let ((dir (create-directory answer)))
                    (if (file-directory? answer)
                        (set! SPAN-download-target-dir answer)
                        #f))))

(define SPAN-build-dir (string-append SPAN-build-and-cache-dir "/" "build")
(SPAN-question~ SPAN-shell-droptext-3
                "Directory where the build process takes place?"
                ""
                SPAN-download-target-dir
                (lambda (answer)
                  (let ((dir (create-directory answer)))
                    (if (file-directory? answer)
                        (set! SPAN-build-dir answer)
                        #f))))

(define SPAN-config "no")
(SPAN-question~ SPAN-shell-droptext-4
                "Always commit changes to config variables to disk?"
                ""
                SPAN-config
                (lambda (answer)
                  (set! SPAN-config answer)
                  #f))

(define SPAN-build-Mb 100)
(SPAN-question~ SPAN-shell-droptext-5
                "Cache size for build directory (in MB)?"
                ""
                SPAN-build-Mb
                (lambda (answer)
                  (set! SPAN-build-Mb answer)
                  #f))

(define SPAN-expire 1)
(SPAN-question~ SPAN-shell-droptext-6
                "Let the index expire after how many days?"
                ""
                SPAN-expire
                (lambda (answer)
                  (set! SPAN-expire answer)
                  #f))

(define SPAN-scan-cache "atstart")
(SPAN-question~ SPAN-shell-droptext-7
                "Perform cache scanning (atstart or never)?"
                ""
                SPAN-scan-cache
                (lambda (answer)
                  (set! SPAN-scan-cache answer)
                  #f))

(define SPAN-cache-metadata "yes")
(SPAN-question~ SPAN-shell-droptext-8
                "Cache metadata (yes/no)?"
                ""
                SPAN-cache-metadata
                (lambda (answer)
                  (set! SPAN-cache-metadata answer)
                  #f))

(define SPAN-policy-building "ask")
(SPAN-question~ SPAN-shell-droptext-9
                "Policy on building prerequisites (follow, ask or ignore)? [ask]"
                ""
                SPAN-policy-building
                (lambda (answer)
                  (set! SPAN-policy-building answer)
                  #f))

;; question 10 is under dev
;; question 11 is under dev
;; question 12 is under dev
;; question 13 is under dev

;; ... until 20

(define SPAN-mirror-url "ask")
(SPAN-question~ SPAN-shell-droptext-20
                "Please enter the URL of your CPAN mirror "
                ""
                SPAN-mirror-url
                (lambda (answer)
                  (set! SPAN-mirror-url answer)
                  #f))

(define SPAN-mirror-url-2 "")
(SPAN-question~ SPAN-shell-droptext-21
                "Enter another URL or RETURN to quit: [] "
                ""
                SPAN-mirror-url-2
                (lambda (answer)
                  (set! SPAN-mirror-url-2 answer)
                  #f))

(display SPAN-shell-droptext-22)
(do ((s (read)(read))
     ((and (symbol? s)
           (string<=? (symbol->string s)(string #\return)))
      0)
     #t))

(display "Signing off - rest is under dev"))