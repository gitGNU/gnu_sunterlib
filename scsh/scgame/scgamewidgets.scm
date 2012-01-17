;;; scgamewidgets.scm - a scheme game library (needs scx-0.2 or scheme48-fb)
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

(load "scgame.scm")

;; for inits see scgame.scm

(define (make-scgame-widget)
  (define (draw)
    (display-msg "subclass responsability"))

  (lambda (msg)
    (cond ((eq? msg 'draw) draw)
          (else
           (display-msg "subclass responsability")))))

(define (make-button-widget)
  (let ((widget (make-scgame-widget))
	(image #f) ;; pixel array
        (pressed-image #f) ;; pixel array
        (pressed #f)
        (width 0)
        (height 0))

    (define (set-image filename)
      (set! image (((make-scimage2)'load-image) filename))
      (let ((wh (vector-ref (list->vector image) 1)))
        (set! width (car wh))
        (set! height (cadr wh))
        ))

    (define (set-pressed-image filename)
      (set! pressed-image (((make-scimage2)'load-image) filename))
      (let ((wh (vector-ref (list->vector image) 1)))
        (set! width (car wh))
        (set! height (cadr wh))
        ))

    (define (draw-pressed-image dpy win gc)
      (init-sync-x-events dpy)
      (map-window dpy win)
      (call-with-event-channel
       dpy win (event-mask exposure button-press)
       (lambda (channel)
         (let loop ()
           (if
            (let ((e (receive channel)))
              (cond
               ((expose-event? e)
                (clear-window dpy win)
                (draw-points dpy win gc (* width height) 0 0
                             (/ width 2) (/ height 2))
                )

               (else #f)))
            (loop))))))

    (define (draw-image dpy win gc)
      (init-sync-x-events dpy)
      (map-window dpy win)
      (call-with-event-channel
       dpy win (event-mask exposure button-press)
       (lambda (channel)
         (let loop ()
           (if
            (let ((e (receive channel)))
              (cond
               ((expose-event? e)
                (clear-window dpy win)
                (draw-points dpy win gc (* width height) 0 0
                             (/ width 2) (/ height 2))
                )

               (else #f)))
            (loop))))))

    (define (draw-points dpy win gc count x y)
      (if (zero? (modulo count 100))
          (display-flush dpy))
      (if (not (zero? count))
          (let ((xf (floor (* (+ 1.2 x) width))) ; These lines center the picture
                (yf (floor (* (+ 0.5 y) height))))
            (draw-point dpy win gc (inexact->exact xf) (inexact->exact yf))
            (draw-points dpy win gc ;; FIXME1
                         (- count 1)
                         (- (* y (+ 1 (sin (* 0.7 x))))
                            (* 1.2 (sqrt (abs x))))
                         (- 0.21 x)
                         width height))))

    (define (draw)
      (if pressed
          (draw-image)
          (draw-pressed-image))
      (map-window dpy win))

    (define (press!)
      (set! pressed #t))

    (define (release!)
      (set! pressed #f))

    (lambda (msg)
      (cond ((eq? 'set-image) set-image)
            ((eq? 'set-pressed-image) set-pressed-image)
            ((eq? 'press!) press!)
            ((eq? 'release!) release!)
	    (widget msg)
	    ))))
