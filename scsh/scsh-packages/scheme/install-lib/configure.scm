;;; Library to obtain information about the underlying platform.
;;; $Id: configure.scm,v 1.1 2004/03/11 19:01:40 acarrico Exp $

(define-structure configure (export host)
  (open scheme-with-scsh
        srfi-13)
  (begin
    (define (canonical-machine uname-record)
      (let* ((machine (uname:machine uname-record))
             (os (uname:os-name uname-record)))
        (cond
         ((member machine '("i386" "i486" "i586" "i686")) "i386")
         ((or (string=? machine "Power Macintosh")
              (and (string=? os "AIX")
                   (regexp-search? (rx (: "00" (= 6 digit) any any "00"))
                                   machine)))
          "powerpc")
         (else machine))))

    (define (canonical-os-name uname-record)
      (string-downcase (uname:os-name uname-record)))

    (define (host)
      (let ((uname-record (uname)))
        (string-append (canonical-machine uname-record)
                       "-"
                       (canonical-os-name uname-record))))))
