(define (interface-value-names interface-name)
  (let ((interface (environment-ref (config-package) interface-name))
	(value-names '()))
    (for-each-declaration
     (lambda (name base-neme type)
       (if (not (equal? type syntax-type))
	   (set! value-names (cons name value-names))))
     interface)
    value-names))

(define-record-type rt-structure :rt-structure
  (make-rt-structure meta-structure)
  rt-structure?
  (meta-structure rt-structure-meta-structure))

(define (rt-structure-loaded? rt-structure)
  (package-loaded? 
   (structure-package (rt-structure-meta-structure rt-structure))))

(define-record-discloser :rt-structure
  (lambda (s)
    (list 'rt-stucture (structure-name (rt-structure-meta-structure s)))))

(define (reify-structure name)
  (let ((struct (get-structure name)))
    (make-rt-structure struct)))

(define (load-structure rts)
  (ensure-loaded (rt-structure-meta-structure rts)))

(define (rt-structure-binding structure name)
  (if (not (rt-structure-loaded? structure))
      (error "Structure not loaded" structure))
  (contents
   (binding-place
    (generic-lookup (rt-structure-meta-structure structure)
		    name))))

(define (load-config-file file)
  (load file (config-package)))

;(define-structure t (export b)
;  (open scheme
;	signals
;	define-record-types
;	primitives)
;  (begin
;    (define-record-type bar :bar
;      (make-bar i)
;      bar?
;      (i bar-i))
;    (define a-bar (make-bar "kjk"))
;    (add-finalizer! :bar (lambda (a-bar) (warn "finalized a bar")))))

      
	  