(define-interface file-modes-interface
  (export file-mode?
	  (file-mode :syntax)
	  file-mode+ file-mode-
	  file-mode=? file-mode<=? file-mode>=?
	  file-mode->integer integer->file-mode))
