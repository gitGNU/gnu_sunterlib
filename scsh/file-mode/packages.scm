(define-structure file-modes file-modes-interface
  (open scheme
	define-record-types
	signals
	bitwise)
  (for-syntax (open scheme bitwise))
  (files file-mode))