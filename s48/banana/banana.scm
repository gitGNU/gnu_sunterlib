;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2003 by Taylor Campbell
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;;;;; - Conditions -

(define-condition-type 'banana-error '(error))
(define banana-error? (condition-predicate 'banana-error))
;; BANANA-ERROR conditions contain information about who signalled
;; them in their CADRs.
(define banana-error-caller cadr)

(define-condition-type 'banana:unknown-byte '(banana-error))
(define unknown-byte-error?
  (condition-predicate 'banana:unknown-byte?))
;; See the note about BANANA-ERROR conditions.  For that reason,
;; and that BANANA:UNKNOWN-BYTE is a subtype of BANANA-ERROR, all
;; the information in BANANA:UNKNOWN-BYTE conditions (and all the
;; others below BANANA-ERROR) store their own fields in the CDDRs.
(define unknown-byte-error-byte caddr)
(define unknown-byte-error-profile cadddr)

(define-condition-type 'banana:unsupported-type '(banana-error))
(define unsupported-type-error?
  (condition-predicate 'banana:unsupported-type))
(define unsupported-type-error-type caddr)
(define unsupported-type-error-value cadddr)

(define-condition-type 'read-eof-error '(read-error))
(define read-eof-error? (condition-predicate 'read-eof-error))

;;;;;; - Utility functions. -

;; Used in NONE-ENCODER/STRING.
(define (map-string->byte-vector f s . rest)
  (let* ((len (string-length s))
         (new (make-byte-vector len 0)))
    (do ((i 0 (+ i 1)))
        ((= i len) new)
      (byte-vector-set! new i
                        (f (string-ref s i))))))

;; Used in NONE-ENCODER/LIST.
(define (byte-vector-concatenate bvectors)
  (let* ((len (fold (lambda (bv counter)
                      (+ (byte-vector-length bv) counter))
                    0 bvectors))
         (new (make-byte-vector len 0)))
    (let loop1 ((to 0) (bvectors bvectors))
      (if (null? bvectors)
          new
          (let* ((bv (car bvectors))
                 (from-len (byte-vector-length bv)))
            (let loop2 ((to to) (from 0))
              (if (= from from-len)
                  (loop1 to (cdr bvectors))
                  (begin
                    (byte-vector-set!
                     new to (byte-vector-ref bv from))
                    (loop2 (+ to 1) (+ from 1))))))))))

;; Variant of BYTE-VECTOR-CONCATENATE.
(define (byte-vector-append . vecs)
  (if (null? vecs)
      ;; No need to even bother calling BYTE-VECTOR-CONCATENATE.
      (make-byte-vector 0 0)
      (byte-vector-concatenate vecs)))

;; Maybe these and the two above should be done using the
;; SEQUENCES structures that also come with Sunterlib.
(define (byte-vector->string bv)
  (let* ((len (byte-vector-length bv))
         (new (make-string len)))
    (do ((i 0 (+ i 1)))
        ((= i len) new)
      (string-set! new i (ascii->char (byte-vector-ref bv i))))))

(define (string->byte-vector s)
  (let* ((len (string-length s))
         (new (make-byte-vector len 0)))
    (do ((i 0 (+ i 1)))
        ((= i len) new)
      (byte-vector-set! new i (char->ascii (string-ref s i))))))

(define (list->byte-vector l)
  (let* ((len (length l))
         (new (make-byte-vector len 0)))
    (do ((i 0 (+ i 1))
         (l l (cdr l)))
        ((= i len) new)
      (byte-vector-set! new i (car l)))))

;; POSINT->BYTE-VECTOR converts nonnegative integers (the name is
;; a tad misleading, but it's easier to write and say than
;; NONNEGINT->BYTE-VECTOR or something) to byte vectors as
;; specified by the Banana protocol.
;;
;; *FIXME* - Shouldn't this be able to be implemented better than
;; by consing up a list...and then reversing that list...and then
;; converting that list into a byte vector?
;;
;; Tail-recursive, iterative version.
(define (posint->byte-vector int)
  (do ((int int (arithmetic-shift int -7))
       (bytes '() (cons (bitwise-and int #x7f) bytes)))
      ((zero? int) (list->byte-vector (reverse bytes)))))

;; CPS version.
; (define (posint->byte-vector int)
;   (do ((int int (arithmetic-shift int -7))
;        (k (lambda (x) x)
;           (lambda (x) (k (cons (bitwise-and int #x7f) x)))))
;       ((zero? int) (list->byte-vector (k '())))))

;; Linear-recursive version.
; (define (posint->byte-vector int)
;   (list->byte-vecctor
;    (let loop ((int int))
;      (if (zero? int)
;          bytes
;          (cons (bitwise-and int #x7f)
;                (loop (arithmetic-shift int -7)))))))

;; BYTE-VECTOR->POSINT is just like POSINT->BYTE-VECTOR but the
;; other way around.
(define (byte-vector->posint bv)
  (let ((len (byte-vector-length bv)))
    (do ((i 0 (+ i 1))
         (result 0 (+ result (* (byte-vector-ref bv i)
                                (expt 128 i)))))
        ((= i len) result))))

;; REAL->BYTE-VECTOR and BYTE-VECTOR->REAL just return 0.0 and a
;; byte vector of zeros, because I haven't the foggiest idea how
;; to implement them correctly.
(define (real->byte-vector r)
  (make-byte-vector 8 0))

(define (byte-vector->real bv)
  0.0)

(define (prettify-byte b)
  (number->string b 16))

(define (map-byte-vector->list f bv)
  (let ((len (byte-vector-length bv)))
    (do ((i (- len 1) (- i 1))
         (result '() (cons (f (byte-vector-ref bv i)) result)))
        ((negative? i) result))))

(define (prettify-byte-vector bv)
  (map-byte-vector->list prettify-byte bv))

(define alist->integer-table
  (let ((make (make-table-maker = abs)))
    (lambda (alist)
      (let ((table (make)))
        (for-each (lambda (key/value)
                    (table-set! table
                                (car key/value)
                                (cdr key/value)))
                  alist)
        table))))

;;;;;; Here starts the actual Banana code.

(define-record-type profile :profile
  (really-make-profile name encoder decoder-table super-profile)
  profile?
  (name profile-name)
  (encoder profile-encoder)
  (decoder-table profile-decoder-table)
  (super-profile profile-super-profile))

(define (make-profile name encoder decoder-alist)
  (extend-profile #f name encoder decoder-alist))

(define (extend-profile super-profile name encoder decoder-alist)
  (really-make-profile name encoder
                       (alist->integer-table decoder-alist)
                       super-profile))

;; Why did this ever take a variable number of arguments?
; (define extend-profile
;   (case-lambda
;     ((super-profile profile)
;      (really-make-profile (profile-name profile)
;                           (profile-encoder profile)
;                           (profile-decoder-table profile)
;                           super-profile))
;     ((super-profile name encoder decoder-alist)
;      (really-make-profile name encoder
;                           (alist->integer-table decoder-alist)
;                           super-profile))))

;; ETB = Element Type Byte
(define (lookup-etb-decoder byte profile)
  (let loop ((p profile))
    (if p
        (or (table-ref (profile-decoder-table p) byte)
            (loop (profile-super-profile p)))
        (signal 'banana:unknown-byte
                'lookup-etb-decoder
                byte profile))))

(define none-etb/list          #x80)
(define none-etb/posint        #x81)
(define none-etb/string        #x82)
(define none-etb/negint        #x83)
(define none-etb/float         #x84)
(define none-etb/largeposint   #x85)
(define none-etb/largenegint   #x86)

(define none-etb-v/list        (byte-vector none-etb/list))
(define none-etb-v/posint      (byte-vector none-etb/posint))
(define none-etb-v/string      (byte-vector none-etb/string))
(define none-etb-v/negint      (byte-vector none-etb/negint))
(define none-etb-v/float       (byte-vector none-etb/float))
(define none-etb-v/largeposint (byte-vector none-etb/largeposint))
(define none-etb-v/largenegint (byte-vector none-etb/largenegint))

(define none-encoder/list
  (lambda (lst)
    (if (null? lst)
        (byte-vector 0 none-etb/list)
        (byte-vector-concatenate
         (append (list (posint->byte-vector (length lst)))
                 (list none-etb-v/list)
                 (map (lambda (x) (encode x profile/none))
                      lst))))))

(define none-encoder/posint
  (lambda (int)
    (byte-vector-append (posint->byte-vector int)
                        none-etb-v/posint)))

(define none-encoder/string
  (lambda (str)
    (byte-vector-append
     (posint->byte-vector (string-length str))
     (byte-vector none-etb/string)
     (map-string->byte-vector char->ascii str))))

(define none-encoder/negint
  (lambda (int)
    (byte-vector-append (posint->byte-vector (- int))
                        none-etb-v/negint)))

(define none-encoder/float
  (lambda (float)
    (byte-vector-append none-etb-v/float
                        (real->byte-vector float))))

(define none-encoder/largeposint
  (lambda (int)
    (byte-vector-append (posint->byte-vector int)
                        none-etb-v/largeposint)))

(define none-encoder/largenegint
  (lambda (int)
    (byte-vector-append (posint->byte-vector (- int))
                        none-etb-v/largenegint)))

(define none/encode
  (lambda (obj)
    (let ((not-supported
           (lambda (type)
             (signal 'banana:unsupported-type
                     'none/encode
                     type obj))))
      ((cond
        ((number? obj)
         (cond
          ((inexact? obj) none-encoder/float)
          ((integer? obj)
           (if (negative? obj)
               (if (< obj -2147483648)
                   none-encoder/largenegint
                   none-encoder/negint)
               (if (> obj 2147483647)
                   none-encoder/largeposint
                   none-encoder/posint)))
          ((rational? obj) (not-supported "rational"))
          ((real? obj) none-encoder/float)
          ((complex? obj) (not-supported "complex"))
          (else (not-supported "unknown number"))))
        ((list? obj) none-encoder/list)
        ((string? obj) none-encoder/string)
        (else (not-supported "unknown value")))
       obj))))

;; CPS version, if you want it.
; (define none-decoder/list
;   (lambda (bytes inport)
;     (let loop ((len (byte-vector->posint bytes))
;                (k (lambda (x) x)))
;       (if (zero? len)
;           (k '())
;           (loop (- len 1)
;                 (lambda (x)
;                   (k (cons (read-element! inport) x))))))))

;; Linear-recursive version, if you want it.
; (define none-decoder/list
;   (lambda (bytes inport)
;     (let loop ((len (byte-vector->posint bytes)))
;       (if (zero? len)
;           '()
;           (cons (read-element! inport) (loop (- len 1)))))))

(define none-decoder/list
  (lambda (bytes inport)
    (let loop ((len (byte-vector->posint bytes)) (vals '()))
      (if (zero? len)
          (reverse vals)
          (loop (- len 1) (cons (read-element! inport
                                               profile/none)
                                vals))))))

(define none-decoder/posint
  (lambda (bytes inport)
    (byte-vector->posint bytes)))

(define none-decoder/string
  (lambda (bytes inport)
    (let* ((len (byte-vector->posint bytes))
           (new (make-string len)))
      (let loop ((i 0))
        (if (= i len)
            new
            (let ((char (read-char inport)))
              (if (eof-object? char)
                  (signal 'read-eof-error
                          "reached eof"
                          'none-decoder/string
                          inport)
                  (begin
                    (string-set! new i char)
                    (loop (+ i 1))))))))))

(define none-decoder/negint
  (lambda (bytes inport)
    (- (byte-vector->posint bytes))))

(define none-decoder/float
  (lambda (bytes inport)
    (let ((s (make-string 9)))
      (string-set! s 0 (ascii->char none-etb/float))
      (do ((i 1 (+ i 1)))
          ((= i 9))
        (string-set! s i (read-char inport)))
      (byte-vector->real (string->byte-vector s)))))

;; NONE-DECODER/POSINT and NONE-DECODER/LARGEPOSINT really do the
;; same thing -- the only difference is that they're called in
;; difference circumstances.
(define none-decoder/largeposint none-decoder/posint)

;; The same can be said of NONE-DECODER/NEGINT and
;; NONE-DECODER/LARGENEGINT.
(define none-decoder/largenegint none-decoder/negint)

(define profile/none
  (make-profile "none" none/encode
    `((,none-etb/list        . ,none-decoder/list)
      (,none-etb/posint      . ,none-decoder/posint)
      (,none-etb/string      . ,none-decoder/string)
      (,none-etb/negint      . ,none-decoder/negint)
      (,none-etb/float       . ,none-decoder/float)
      (,none-etb/largeposint . ,none-decoder/largeposint)
      (,none-etb/largenegint . ,none-decoder/largenegint))))

(define (etb? b)
  (> b 127))

(define (read-element! inport profile)
  (let loop ((bytes '()))
    (let ((current-char (read-char inport)))
      (if (eof-object? current-char)
          (signal 'read-eof-error
                  "reached EOF"
                  'read-element!
                  inport)
          (let ((current-byte (char->ascii current-char)))
            (if (etb? current-byte)
                ((lookup-etb-decoder current-byte profile)
                 (apply byte-vector (reverse bytes))
                 inport)
                (loop (cons current-byte bytes))))))))

(define (decode x . profile)
  (let ((profile (if (pair? profile) (car profile) profile/none)))
    (cond
     ((input-port?  x) (read-element! x profile))
     ((string?      x) (read-element! (make-string-input-port x)
                                      profile))
     ((byte-vector? x) (decode (byte-vector->string x) profile))
     (else (error "decode: can't decode from source" x)))))

(define (encode obj . profile)
  (let ((f (profile-encoder (if (pair? profile)
                                (car profile)
                                profile/none))))
    (f obj)))
