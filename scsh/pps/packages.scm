(define-interface pps-interface
  (export  pps

	   process-info?
	   process-info-pid
	   process-info-ppid
	   process-info-logname
	   process-info-real-uid
	   process-info-effective-uid
	   process-info-saved-set-uid
	   process-info-real-gid
	   process-info-effective-gid
	   process-info-saved-set-gid
	   process-info-time
           process-info-%cpu
	   process-info-tty
	   process-info-executable
	   process-info-command-line))

(define-structure pps pps-interface
  (open scheme-with-scsh
	(subset srfi-1 (drop take))
	define-record-types)
  (files pps))
