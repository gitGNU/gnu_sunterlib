;;; read and write bytes (instead of characters)
;;;   READ/PEEK/WRITE-BYTE are simple wrappers around the corresponding
;;; character procedures, therefore not optimally efficient.
;;;   READ-BYTES!/PARTIAL

;; [input-port | fdes] -> eof | integer in [0:256)
(define (read-byte . maybe-in)
  (let ((x (apply read-char maybe-in)))
    (if (eof-object? x) x
	(char->ascii x))))

;; [input-port | fdes] -> eof | integer in [0:256)
(define (peek-byte . maybe-inport)
  (let ((x (apply peek-char maybe-inport)))
    (if (eof-object? x) x
	(char->ascii x))))

;; integer -> any
;; assume B in [0:256)
;; write byte with numerical value B to you know what
(define (write-byte b . maybe-out)
  (apply write-char (ascii->char b) maybe-out))


;; requires i/o for read-block, debug-utils for assert
;; Assume MIN <= UBERMAX and BUF length < START + UEBERMAX.
;; Read at least MIN bytes from INPORT into BUF starting at index START,
;; possibly more if possible w/o blocking, but in toto less than UEBERMAX.
;; Return the number of bytes (<= UEBERMAX) read into BUF, but eof if
;; min>0 and reading started at end of file.
;; [ Problem with the underlying READ-BLOCK:  if we want to read (at most)
;;   n>0 bytes w/o blocking, we have to communicate the upper bound n by
;;   the buffer size.  READ-BOLLOCK prefers to read maybe less than
;;   available but to avoid allocating a temp buffer ]
(define (read-bollock buf start min uebermax inport)
  (assert (<= 0 min uebermax) read-bollock)
  (assert (<= (+ start uebermax)
              (sequence-length buf))
          read-bollock)
  (let* ((len (sequence-length buf))
         (mean (if (= min 0) 0
                   (read-block buf start min inport)))
         (xtra (cond ((eof-object? mean) mean)
                     ((< (+ start uebermax) len) 0)
                     (else
                      ;; the remaining buffer size communicates
                      ;; the max number of bytes to be read
                      ;; (if UEBERMAX = MIN we are asking for 0 bytes)
                      (read-block buf (+ start mean) 'immediate inport)))))

    (if (eof-object? xtra) mean
        (+ mean xtra))))



;; like READ-STRING!/PARTIAL with byte-vectors instead of strings
;; [ accepts strings as well ]
(define (read-bytes!/partial bytes . args)
  (let-optionals args ((fd/port (current-input-port))
		       (start   0)
		       (end     (sequence-length bytes)))
    (assert (<= 0 start end (sequence-length bytes))
            read-bytes!/partial)
    (cond ((= end start) 0)
          ((integer? fd/port)
           (let ((port (fdes->inport fd/port)))
             (set-port-buffering port bufpol/block (- end start))
             (read-bytes!/partial bytes port start end)))
          (else
           (let ((blocking-i/o? (= 0 (bitwise-and open/non-blocking
                                                  (fdes-status fd/port)))))
             (read-bollock bytes start
                           (if blocking-i/o? 1 0) ; cond. forward progress
                           (- end start)
                           fd/port))))))


(define (read-bytes/partial len . maybe-fd/port)
  (assert (<= 0 len)
          read-bytes/partial)
  (let* ((fd/port (:optional maybe-fd/port (current-input-port)))
         (buf (make-byte-vector len 0))
         (nread (read-bytes!/partial buf fd/port 0 len)))
    (cond ((not nread) #f)
          ((= nread len) buf)
          (else (subsequence buf 0 nread)))))


;; The implementation of READ-STRING! should work with byte-vectors but for
;; BOGUS-SUBSTRING-SPEC? -- which calls STRING-LENGTH
;; READ-BYTES! accepts strings, too.
(define (read-bytes! bytes . args)
  (let-optionals args ((fd/port (current-input-port))
		       (start   0)
		       (end     (sequence-length bytes)))
    (assert (<= 0 start end (sequence-length bytes))
            read-bytes!)
    (if (integer? fd/port)
        (let ((port (fdes->inport fd/port)))
          (set-port-buffering port bufpol/block (- end start))
          (read-block bytes start (- end start) port))
        (let ((nbytes/eof (read-block bytes start (- end start) fd/port)))
          (if (eof-object? nbytes/eof) #f
              nbytes/eof)))))


(define (read-bytes len . maybe-fd/port)
  (assert (<= 0 len) read-bytes)
  (let* ((fd/port (:optional maybe-fd/port (current-input-port)))
         (buf (make-byte-vector len 0))
         (nread (read-bytes! buf fd/port 0 len)))
    (cond ((not nread) #f)
          ((= nread len) buf)
          (else (subsequence buf 0 nread)))))


(define (write-bytes/partial bytes . args)
  (let-optionals args ((fd/port (current-output-port))
		       (start 0)
		       (end (sequence-length bytes)))
    (assert (<= 0 start end (sequence-length bytes))
            write-bytes/partial)
    (error
"write-bytes/partial : behaves no better than write-string/partial.
The latter is dereleased, cf. the RELEASE notes."
)))

;; the implementation of WRITE-STRING should write byte-vectors but for
;; the implicit STRING-LENGTH call ...  WRITE-BYTES should write strings,
;; too.
(define (write-bytes bytes . args)
  (let-optionals args ((fd/port (current-output-port))
		       (start   0)
		       (end     (sequence-length bytes)))
    (assert (<= 0 start end (sequence-length bytes))
            write-bytes)
    (let ((port (if (integer? fd/port)
                    (let ((port (fdes->outport fd/port)))
                      (set-port-buffering port bufpol/block (- end start))
                      port)
                    fd/port)))
      (write-block bytes start (- end start) port))))
