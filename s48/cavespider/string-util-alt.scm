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

(load "file-util.scm")
(load "hash-util.scm")
(load "html-util.scm")

;;(define (url->hostname url-list hostname-list)
;;  (let ((file-contents (file-contents->url )))
;;    ))


(define (tags filename)
  (html-tags filename))

(define (file-contents->url tags-of-file-contents-str)
  ;;(display tags-of-file-contents-str)
  (let ((s "")
        (ret '())
        (http-prefix "http://"))

    (do ((i 0 (+ i 1)))
        ((>= i (string-length tags-of-file-contents-str))
         ret)
      (cond ((not (eq? #\h (string-ref tags-of-file-contents-str i)))
             (set! s ""))
            ((eq? #\h (string-ref tags-of-file-contents-str i))

             (let ((s2 ""))

               (do ((j i (+ j 1)))
                   ((cond ((string=? s2 http-prefix)

                           (let ((s3 ""))
                             (do ((k j (+ k 1)))
                                 ((cond ((eq? (string-ref tags-of-file-contents-str k)
                                              #\");;FIXME

                                         (set! j k)(set! i k)(set! s2 ""))
                                        ((eq? (string-ref tags-of-file-contents-str k)
                                              #\/)
                                         (set! ret (append ret (list s3)))

                                         (set! j k)
                                         (set! i k))
                                        ((>= k (string-length tags-of-file-contents-str));;FIXME prev
                                         (set! s2 "")(set! i k)(set! j k))
                                        ))
                               (set! s3 (string-append
                                         s3
                                         (string (string-ref tags-of-file-contents-str k))))
                             )

                             ))
                          ((not (string<=? s2 http-prefix))
                           (set! s "")

                           (set! i j))
                          ))

                 (set! s2 (string-append
                           s2
                           (string (string-ref tags-of-file-contents-str j))))
                 (display "s2=")(display s)

               (set! i j)))))
      (set! s (string-append s (string (string-ref tags-of-file-contents-str i))))
      ;;(display "s=")(display (string-ref tags-of-file-contents-str i))
      )
    ret))


(display
 (file-contents->url (html-dump "index.html"))
 )
