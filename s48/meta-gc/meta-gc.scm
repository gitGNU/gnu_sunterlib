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

(load "gc.scm")
(load "gc-sized.scm")
(load "gc-sized-fast.scm")

(define (make-meta-gc gc-actor-maker)
  (let ((*gc-program (gc-actor-maker))
	)
    
    ;; interrupt system : FIXME sort of hooks
    (define (signal number)
      (cond ((= number 9)
	     ;;(display "exiting meta-gc...")
	     ;;,exit
	     0)
	    ((= number 7)
	     ;;(display "exiting meta-gc...")
	     ;;,exit
	     0)
	    (else (display "make-meta-gc : SIGNAL == ")(display number)(display " unknown signal to gc")
		  (newline)	
		  )))

    (define (run)
      0)

    (define (dispatch msg)
      (cond ((eq? msg 'run) run)
	    ((eq? msg 'signal) signal)
	    ((eq? msg 'int) signal)
	    ((eq? msg 'interrupt) signal)
	    
	    (else (display "make-meta-gc : message not understood : ")(display msg)(newline)
		  )))
      dispatch)) 
