#! /bin/sh
exec scsh -o filenames -s "$0" "$@"
!#

;;; details.scm
;;;
;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2003 by Anthony Carrico
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define s48-dirs
  (run/strings (find s48
                     -maxdepth 1
                     -mindepth 1
                     -type d
                     ! -name CVS
                     ! -name rt-modules)))

(define scsh-dirs
  (run/strings (find scsh
                     -maxdepth 1
                     -mindepth 1
                     -type d
                     ! -name CVS)))

(define entry
  (lambda (dir)
    (with-current-input-port
     (open-input-file (string-append dir "/BLURB"))
     (let loop ()
       (let ((ch (read-char)))
         (if (eof-object? ch)
             (values)
             (begin
               (write-char ch)
               (loop))))))
    (display "Authors: ")
    (with-current-input-port
     (open-input-file (string-append dir "/AUTHORS"))
     (let loop ()
       (let ((ch (read-char)))
         (if (eof-object? ch)
             (values)
             (begin
               (write-char ch)
               (loop))))))
    (newline)))

;; Create the details file from the AUTHORS and BLURB files.
(with-current-output-port
 (open-output-file "DETAILS")
 (display "S48 LIBRARIES\n\n")
 (for-each entry s48-dirs)
 (display "SCSH LIBRARIES\n\n")
 (for-each entry scsh-dirs))
