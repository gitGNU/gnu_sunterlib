;
; COUT, CERR, NL, ++ from Oleg Kiselyov's myenv.scm
; changes:
;   ##stderr --> (error-output-port)
;   ++ via define-syntax and +

(define-syntax define-structure
  (lambda (form rename name=)
    (let ((name (cadr form))            ; Symbol | Generated-Name
          (fields (cddr form))          ; Proper-List(S. | G.N.)
          (concat (lambda (symbols)     ; Proper-List(S.) -> S.
                    (string->symbol
                     (apply string-append
                            (map symbol->string symbols))))))

      `(,(rename 'define-record-type)
        ,name
        (,(concat `(make- ,name)) ,@fields)
        ,(concat `(,name ?))
        ,@(map (lambda (field)
                 `(,field
                   ,(concat `(,name - ,field))
                   ,(concat `(,name - ,field -set!))))
               fields))
      )))


; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
                  (x (error-output-port))
                  (display x (error-output-port))))
            args))

(define nl (string #\newline))


;; read-only increment
(define-syntax ++
  (syntax-rules ()
    ((++ x)
     (+ 1 x))))
