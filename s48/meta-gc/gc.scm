;; Copyright (C) Johan Ceuppens 2013
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One chunk per memory allocation, can be used in meta-gc.scm API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-gc)
  (let ((*heap '())
	(*null '())
	(*max-datasize 1024)	
	(*datasize 0)	
	(*dynamic-data '())	
	)
    
    (define (malloc)
      (add-to-heap! 1)
      )
    (define (calloc nchunks)
      (add-to-heap! nchunks)
      )


    ;; private procedures
    
    (define (get-heap) *heap)
    
    ;; data is a string
    (define (get-data-size d) (string-length d))

    (define (generate-error msg)
      (cond ((eq? msg 'memory-exhausted)
	     (display "memory exhausted")(newline))
	    ))
    
    (define (add-to-heap! n)
      (set! *heap (append (get-heap) (list (make-chunk)))))

    (define (make-chunk)
      (cons 'chunk *null))  

    (define (set-data! chunk data)
      (set-cdr! chunk data))
    
    (define (set-chunk-data! chunk)
      (set! *datasize (+ *datasize (get-data-size chunk)))
      (if (> *datasize *max-datasize) 
	  (generate-error 'memory-exhausted)
	  ;; The data is dynamically bound in this actor
	  (set-data! chunk *dynamic-data)
	  ))
  
  (define (dispatch msg)
    (cond ((eq? msg 'malloc) malloc)	
	  ((eq? msg 'calloc) calloc)
	  (else (display "make-gc : message not understood : ")(display msg)(newline))
	  ))
  dispatch))
