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

(load "meta-gc.scm")

(define (make-gc-hook)
  (define (add-hook)
    ;;FIXME fill in SMOB system in C
    #f)
  
  (define (dispatch msg)
    (cond ((eq? msg 'add-hook) add-hook)
	  (else (display "make-gc-hook : message not understood : ")(display msg)(newline)
		)))
  
  dispatch)
