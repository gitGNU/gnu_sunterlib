(define-interface file-modes-interface
  (export file-mode?
	  (file-mode :syntax)
	  file-mode+ file-mode-
	  file-mode=? file-mode<=? file-mode>=?
	  file-mode->integer integer->file-mode))

(define-structure file-modes file-modes-interface
  (open scheme
	define-record-types
	signals
	bitwise)
  (for-syntax (open scheme bitwise))
  (files file-mode))
