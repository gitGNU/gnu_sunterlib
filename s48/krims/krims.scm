; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

; Odds and Ends
;   that haven't found a natural place, yet.
;
; Synopses
;
;   (assert [id] exp)                                             ; syntax
;   If not EXP signal an error with suitable message.  The optional 
;   ID may be any printable object, e.g. a symbol naming the enclosing
;   procedure.  [ This could be done with a procedure, but ASSERT being
;   a macro, we can redefine it as the trivial form that doesn't evaluate
;   its parameters. ]
;
;   (receive/name loop formals exp form0 ...)                     ; syntax
;   Bind LOOP to a macro wrapped around the procedure LUP with parameter
;   list FORMALS and body FORM0 ... so that
;   * (LOOP multi-valued-expression) calls LUP with the values of 
;     multi-valued-expression , and
;   * (LOOP exp0 ...) becomes (LUP exp0 ...)
;
;   (gen-dispatch ((predicate action) ...) e0 e1 ... en)          ; syntax
;   Dispatch action on type of first argument E0:  feed E0 ... EN to the 
;   first action such that the PREDICATE holds for E0.  Signal an error 
;   if nothing goes.


(define-syntax assert
      (syntax-rules ()
	((assert ?x)
	 (if (not ?x) (error "Assertion failed" '?x)))
        ((assert ?tag ?x)
         (if (not ?x) (error (format #f "~a -- assertion failed" ?tag)
                             '?x)))))



; RECEIVE/NAME is a multiple values analogue of named LET. 
; Syntax:  (receive/name <identifier> <formals> <expression> <body>)
;   [ non-terminals as in R5RS ]
; Semantics:  (receive/name loop (x y) exp0     ; yes, it's a special case
;               ... (loop exp1) ...) 
; is eqv to
;             (receive (x y) exp0
;               (let lup ((x x) (y y))
;                 ... (receive (x y) exp1
;                       (lup x y)) ...))
;
; And         (receive/name loop (x y) exp0
;               ... (loop exp1 exp1) ...)
; is eqv to 
;             (receive (x y) exp0
;               (let lup ((x x) (y y))
;                 ... (lup exp1 exp2) ...))
;
; Absurd example:
; (define (shove n xs) (values (- n 1) (cons n xs)))
; (receive/name loop (n xs) (values 7 '()) 
;   (if (= n 0)
;       (display xs)
;       (loop (shove n xs))))
(define-syntax receive/name
  (syntax-rules ()
    ((_ ?tag ?tuple ?call ?body0 ?body1 ...)
     (letrec ((proc
               (lambda ?tuple
                 (let-syntax
                     ((?tag (syntax-rules ()
                              ((?tag ?e)
                               (call-with-values (lambda () ?e)
                                 (lambda ?tuple (proc . ?tuple))))
                              ((?tag . ?args)
                               (proc . ?args)))))
                   ?body0 ?body1 ...))))
       (call-with-values (lambda () ?call) proc)))))


;; dispatch on type of the first argument
;; [ should we support a default clause (else ?proc) ? ]
(define-syntax gen-dispatch
  (syntax-rules ()
    ((_ () ?x0 . ?rest)
     #f)
    ((_ ((?pred ?proc) ...) ?x0 . ?rest)
     (cond ((?pred ?x0) (?proc ?x0 . ?rest))
           ...
           (else (error "unsupported input type" ?x0))))))
