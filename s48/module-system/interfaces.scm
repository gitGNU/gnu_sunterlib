(define-interface rt-modules-interface
  (export ((lambda-interface 
	    with-names-from-rt-structure)
	   :syntax)
	  reify-structure
          rt-structure->environment
	  load-structure
	  load-config-file
	  rt-structure-binding))

(define-interface rt-modules-core-interface
  (export interface-value-names 
	  reify-structure
          rt-structure->environment
	  load-config-file
	  rt-structure-binding
	  load-structure))