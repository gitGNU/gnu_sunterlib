;;; Installation library for scsh modules.
;;; $Id: install-lib.scm,v 1.1 2004/03/11 19:01:40 acarrico Exp $

;; TODO
;; - add a "--debug" option
;; - add support for communication between configure and pkg-def.scm
;; - add support for image creation
;; - add support to maintain a documentation index

;;
;; Support code templates
;;
;; These templates are meant to be inserted in package-loading
;; scripts.

;; Template to parse libtool's ".la" files.
(define tmpl-libtool-la-reader
  '((define (normalize-la-entry key val)
      (let ((left-quotes-rx (rx (: bos #\')))
            (right-quotes-rx (rx (: #\' eos)))
            (kill-matches
             (lambda (rx str)
               (regexp-substitute/global #f rx str 'pre 'post))))
        (cons (string->symbol key)
              (kill-matches left-quotes-rx
                            (kill-matches right-quotes-rx val)))))
    (define add-la-entry
      (let ((splitter (infix-splitter (rx #\=)))
            (comment-rx (rx (: bos #\#))))
        (lambda (line alist)
          (cond
           ((and (not (regexp-search? comment-rx line))
                 (string-index line #\=))
            (let ((lst (splitter line)))
              (if (= 2 (length lst))
                  (cons (apply normalize-la-entry lst) alist)
                  (error "Could not read la entry" line list))))
           (else alist)))))
    (define (read-libtool-la file-name)
      (call-with-input-file
          file-name
        (lambda (port)
          (let lp ((line (read-line port)) (alist '()))
            (if (eof-object? line)
                alist
                (lp (read-line port) (add-la-entry line alist)))))))))

;;
;; Utilities
;;

(define default-perms-fn
  (lambda (name) #o755))

;; Return the name of the parent directory of FNAME.
(define (parent-directory fname)
  (file-name-directory (directory-as-file-name fname)))

;; Create directory FNAME and all its parents, as needed.
(define (create-directory&parents fname . rest)
  (let-optionals rest ((perms-fn default-perms-fn))
    (let ((parent (parent-directory fname)))
      (if (not (file-exists? parent))
          (apply create-directory&parents parent rest))
      (if (not (file-exists? fname))
          (-create-directory fname
                             (perms-fn (absolute-file-name fname)))))))

;; Return the length of the longest prefix common to lists L1 and L2,
;; by comparing elements using PRED (defaults to EQUAL?).
(define (common-prefix-length l1 l2 . rest)
  (let-optionals rest ((pred equal?))
    (if (or (null? l1) (null? l2) (not (pred (first l1) (first l2))))
        0
        (+ 1 (apply common-prefix-length (cdr l1) (cdr l2) rest)))))

;; Return the name of file NAME relative to DIR (defaults to current
;; directory).
(define (relative-file-name name . rest)
  (let-optionals rest ((dir (cwd)))
    (let* ((abs-pl (split-file-name (absolute-file-name name)))
           (dir-pl (split-file-name (directory-as-file-name dir)))
           (cp-len (common-prefix-length abs-pl dir-pl)))
      (path-list->file-name (append (make-list (- (length dir-pl) cp-len) "..")
                                    (drop abs-pl cp-len))))))

;; Return the name of FNAME, which must be absolute, with NEW-ROOT as
;; root.
(define (re-root-file-name fname new-root)
  (let ((fname-pl (split-file-name fname))
        (new-root-pl (split-file-name new-root)))
    (if (string=? (first fname-pl) "")
        (path-list->file-name (append new-root-pl (cdr fname-pl)))
        (error "no root to replace in relative file name" fname))))

;; If FILE exists, fail if --force was not given, delete it otherwise.
(define (delete-file-or-fail file)
  (if (file-exists? file)
      (if (get-option-value 'force)
          (-delete-file file)
          (error "target file exists" file))))

;; Copy file/symlink SOURCE to TARGET. TARGET must be the name of a
;; non-existing file (i.e. it cannot be the name of a directory).
(define (copy-file source target)
  (delete-file-or-fail target)
  (if (file-symlink? source)
      (create-symlink (read-symlink source) target)
      (begin
        (run (cp ,source ,target))
        (set-file-mode target (file-mode source)))))

;; Like "load" but without printing anything.
(define load-quietly
  (let ((eval (lambda (expr t) (eval expr (interaction-environment)))))
    (lambda (file-name)
      (call-with-input-file file-name
        (lambda (port) (port-fold port read eval #f))))))

(define (permissions->string perms)
  (let ((decode (lambda (mask str)
                  (if (zero? (bitwise-and perms mask)) "-" str))))
    (string-append (decode #o400 "r") (decode #o200 "w") (decode #o100 "x")
                   (decode #o040 "r") (decode #o020 "w") (decode #o010 "x")
                   (decode #o004 "r") (decode #o002 "w") (decode #o001 "x"))))

;; Replace all bindings of KEY in ALIST with one binding KEY to DATUM.
(define (alist-replace key datum alist)
  (alist-cons key datum (alist-delete key alist)))

;; Add all mappings from ALIST-2 to ALIST-1. If a key is mapped in
;; both lists, the mapping in the first list takes precedence.
(define (alist-combine alist-1 alist-2)
  (fold (lambda (key/value result)
          (if (assoc (car key/value) result) result (cons key/value result)))
        alist-1
        alist-2))

;; Return the value associated with KEY in ALIST. If none exists,
;; return DEFAULT, or signal an error if no DEFAULT was given.
(define (alist-get key alist . rest)
  (cond ((assoc key alist) => cdr)
        ((not (null? rest)) (first rest))
        (else (error "internal error: cannot find key in alist" key alist))))

;; Convert all arguments to strings using DISPLAY and concatenate the
;; result in a single string which is returned.
(define (as-string . args)
  (call-with-string-output-port
   (lambda (port) (for-each (lambda (arg) (display arg port)) args))))

;; Return a string of max(M,N) white spaces.
(define (spaces m n) (make-string (max m n) #\space))

;;
;; Support for dry runs / verbose operation.
;;

(define (wrap real-fn info-fn)
  (lambda args
    (if (or (get-option-value 'verbose) (get-option-value 'dry-run))
        (begin (display (apply info-fn args)) (newline)))
    (if (not (get-option-value 'dry-run))
        (apply real-fn args))))

(define -create-directory
  (wrap create-directory
        (lambda (fname . rest)
          (let-optionals rest ((perms #o777))
            (as-string "creating directory " fname
                       " (perms: " (permissions->string perms) ")")))))

(define -create-symlink
  (wrap create-symlink
        (lambda (old-name new-name)
          (as-string "creating symbolic link " new-name
                     " pointing to " old-name))))

(define -copy-file
  (wrap copy-file
        (lambda (source target)
          (as-string "copying file " source " to " target))))

(define -delete-file
  (wrap delete-file
        (lambda (fname) (as-string "deleting file " fname))))

;;
;; Versions
;;
;; Versions are represented as lists of integers, the most significant
;; being at the head.

;; Return the printed representation of VERSION.
(define (version->string version)
  (string-join (map number->string version) "."))

;; Convert the printed representation of a version found in
;; VERSION-STRING to the version it represents.
(define string->version
  (let ((split-version (infix-splitter ".")))
    (lambda (version-string)
      (map string->number (split-version version-string)))))

;; Compare two versions lexicographically and return the symbol
;; 'smaller if the first is strictly smaller than the second, 'equal
;; if both are equal, and 'greater otherwise.
(define (version-compare v1 v2)
  (cond ((and (null? v1) (null? v2)) 'equal)
        ((null? v1) 'smaller)
        ((null? v2) 'greater)
        (else (let ((v1h (car v1)) (v2h (car v2)))
                (cond ((< v1h v2h) 'smaller)
                      ((> v1h v2h) 'greater)
                      (else (version-compare (cdr v1) (cdr v2))))))))

(define (version<? v1 v2) (eq? (version-compare v1 v2) 'smaller))
(define (version>? v1 v2) (eq? (version-compare v1 v2) 'greater))
(define (version=? v1 v2) (eq? (version-compare v1 v2) 'equal))

;;
;; Layouts
;;

;; Names of all shared locations (i.e. the ones which do not depend on
;; the platform).
(define shared-locations
  '(active base misc-shared scheme doc))

;; Names of all non-shared (i.e. platform-dependent) locations.
(define non-shared-locations
  '(lib))

;; All locations defined for a layout.
(define all-locations (append shared-locations non-shared-locations))

;; Return true iff the given location is "active", that is if files
;; should be installed in it.
(define (active-location? location)
  (member location (if (get-option-value 'non-shared-only)
                       non-shared-locations
                       all-locations)))

;; Parse a layout given as a string of comma-separated bindings. A
;; binding consists of the name of a location, followed by an equal
;; sign and the name of the directory to associate to the location.
;; Return #f if parsing fails.
(define parse-layout
  (let ((split-defs (infix-splitter ","))
        (split-sides (infix-splitter "=")))
    (lambda (str)
      (call-with-current-continuation
       (lambda (return)
         (map (lambda (name&value)
                (let ((name/value (split-sides name&value)))
                  (if (= 2 (length name/value))
                      (cons (string->symbol (first name/value))
                            (second name/value))
                      (return #f))))
              (split-defs str)))))))

;; Return an absolute version of LAYOUT by prepending PREFIX to all
;; its components (which must be relative).
(define (absolute-layout layout prefix)
  (map (lambda (key/value)
         (cons (car key/value) (absolute-file-name (cdr key/value) prefix)))
       layout))

;; Return the directory associated with the LOCATION in LAYOUT.
(define (layout-dir layout location)
  (alist-get location layout #f))

;; Predefined layouts

(define (scsh-layout platform base)
  `((base        . ,base)
    (misc-shared . ,base)
    (scheme      . ,(absolute-file-name "scheme" base))
    (lib         . ,(absolute-file-name "lib" base))
    (doc         . ,(absolute-file-name "doc" base))))

(define (scsh-layout-1 platform pkg)
  (alist-combine '((active . "."))
                 (scsh-layout platform (package-full-name pkg))))

(define (scsh-layout-2 platform pkg)
  (alist-combine
   '((active . "active"))
   (scsh-layout platform
                (path-list->file-name
                 (list "installed"
                       (package-name pkg)
                       (version->string (package-version pkg)))))))

(define (fhs-layout platform pkg)
  (let ((base (absolute-file-name (package-full-name pkg)
                                  "share/scsh/modules")))
    `((base        . ,base)
      (misc-shared . ,base)
      (scheme      . ,(absolute-file-name "scheme" base))
      (lib         . ,(absolute-file-name (package-full-name pkg)
                                          "lib/scsh/modules"))
      (doc         . ,(absolute-file-name (package-full-name pkg) "share/doc"))
      (active      . "share/scsh/modules"))))

(define predefined-layouts
  `(("scsh"     . ,scsh-layout-1)
    ("scsh-alt" . ,scsh-layout-2)
    ("fhs"      . ,fhs-layout)))

;; If NAME-OR-LAYOUT refers to a predefined layout, return it.
;; Otherwise, if NAME-OR-LAYOUT is a valid layout definition, parse
;; and return it. Otherwise, return false.
(define (resolve-layout name-or-layout)
  (or (alist-get name-or-layout predefined-layouts #f)
      (parse-layout name-or-layout)))

;;
;; Packages
;;

(define-record-type package
  (make-package name version extensions directory install-thunk)
  package?
  (name package-name)
  (version package-version)
  (extensions package-extensions)
  (directory package-directory)
  (install-thunk package-install-thunk))

;; Return the full name of PKG.
(define (package-full-name pkg)
  (string-append
   (package-name pkg) "-" (version->string (package-version pkg))))

;; Return the value of extension called EXT for PKG. If such an
;; extension doesn't exist, return #f.
(define (package-extension pkg ext)
  (alist-get ext (package-extensions pkg) #f))

;; List of all defined packages
(define *packages* (make-fluid #f))

;; Add PKG to the above list of all defined packages.
(define (add-package pkg)
  (cell-set! (fluid *packages*)
             (cons pkg (cell-ref (fluid *packages*)))))

(define-syntax define-package
  (syntax-rules ()
    ((define-package name version extensions body ...)
     (add-package (make-package name
                                (quasiquote version)
                                (quasiquote extensions)
                                (cwd)
                                (lambda () body ...))))))

;; Load (and evaluate the contents of) the file "pkg-def.scm" in the
;; current directory and return the packages it defines.
(define (load-packages)
  (let-fluid *packages* (make-cell '())
             (lambda ()
               (load-quietly package-definition-file)
               (cell-ref (fluid *packages*)))))

(define (load-package-in dir)
  (with-cwd dir (load-quietly package-definition-file)))

;;
;; Package options
;;

(define-record-type pkg-opt
  (really-make-pkg-opt key
                       help
                       arg-help
                       required-arg?
                       optional-arg?
                       default
                       parse
                       show
                       transform)
  pkg-opt?
  (key pkg-opt-key)
  (help pkg-opt-help)
  (arg-help pkg-opt-arg-help)
  (required-arg? pkg-opt-required-arg?)
  (optional-arg? pkg-opt-optional-arg?)
  (default pkg-opt-default)
  (parse pkg-opt-parse)
  (show pkg-opt-show)
  (transform pkg-opt-transform))

(define (make-pkg-opt key help arg-help req-arg? opt-arg? default . rest)
  (let-optionals rest ((parse identity)
                       (show identity)
                       (transform (lambda (old new) new)))
    (really-make-pkg-opt key
                         help
                         arg-help
                         req-arg?
                         opt-arg?
                         default
                         parse
                         show
                         transform)))

;; Return the name of PKG-OPT
(define (pkg-opt-name pkg-opt)
  (symbol->string (pkg-opt-key pkg-opt)))

;; Convert PKG-OPT into an SRFI-37 option.
(define (pkg-opt->option pkg-opt)
  (let ((key (pkg-opt-key pkg-opt))
        (transform (pkg-opt-transform pkg-opt))
        (parse (pkg-opt-parse pkg-opt)))
    (option (list (pkg-opt-name pkg-opt))
            (pkg-opt-required-arg? pkg-opt)
            (pkg-opt-optional-arg? pkg-opt)
            (lambda (opt name arg alist)
              (alist-replace key
                             (transform (alist-get key alist) (parse arg))
                             alist)))))

;; Return a pair (key, default) which associates the default value of
;; PKG-OPT to its key.
(define (pkg-opt-key&default pkg-opt)
  (cons (pkg-opt-key pkg-opt) (pkg-opt-default pkg-opt)))

;; Return the list of all package options of the PACKAGES.
(define (all-package-options packages)
  (append-map
   (lambda (pkg)
     (cond ((package-extension pkg 'options)
            => (lambda (opts)
                 (map (lambda (args) (apply make-pkg-opt args)) opts)))
           (else '())))
   packages))

;;
;; Load script handling
;;

;; Evaluate THUNK with CURRENT-OUTPUT-PORT opened on the current
;; package's loading script (in the install directory). During a dry
;; run, or when only non-shared data has to be installed, do nothing.
(define (with-output-to-load-script* thunk)
  (let* ((dir (get-directory 'base #t))
         (file (absolute-file-name "load.scm" dir)))
    (create-directory&parents dir)
    (if (not (or (get-option-value 'dry-run)
                 (get-option-value 'non-shared-only)))
        (begin
          (delete-file-or-fail file)
          (with-output-to-file file thunk)))))

;; Sugar for with-output-to-load-script*.
(define-syntax with-output-to-load-script
  (syntax-rules ()
    ((with-output-to-load-script body ...)
     (with-output-to-load-script* (lambda () body ...)))))

;; Pretty-print all the elements of s-exps, one after the other, to
;; the current package's loading script (in the install directory).
(define (write-to-load-script s-exps)
  (with-output-to-load-script (for-each p s-exps)))

;;
;; Actions
;;

;; Perform all actions required to make the given version of the
;; package active (i.e. the default version for that package).
(define (activate-package layout pkg)
  (let ((lnk-name (absolute-file-name (package-name pkg)
                                      (layout-dir layout 'active))))
    (if (and (file-exists? lnk-name) (file-symlink? lnk-name))
        (-delete-file lnk-name))
    (-create-symlink (relative-file-name (layout-dir layout 'base)
                                         (file-name-directory lnk-name))
                     lnk-name)))

(define (install-thing% layout name-or-pair location target-rel-dir perms-fn)
  (let* ((target-dir (absolute-file-name target-rel-dir
                                         (layout-dir layout location)))
         (source (if (pair? name-or-pair) (car name-or-pair) name-or-pair))
         (target-name (file-name-nondirectory (if (pair? name-or-pair)
                                                  (cdr name-or-pair)
                                                  name-or-pair)))
         (target (absolute-file-name target-name target-dir)))
    (if (not ((get-option-value 'exclude) source))
        (begin
          (create-directory&parents target-dir perms-fn)
          (cond ((or (file-regular? source) (file-symlink? source))
                 (-copy-file source target))
                ((file-directory? source)
                 (-create-directory target (file-mode source))
                 (install-directory-contents% layout
                                              source
                                              location
                                              (absolute-file-name
                                               target-name
                                               target-rel-dir)
                                              perms-fn))
                (else (error "cannot install file-system object" source)))))))

(define (install-directory-contents% layout
                                     name
                                     location
                                     target-rel-dir
                                     perms-fn)
  (for-each (lambda (thing)
              (install-thing% layout thing location target-rel-dir perms-fn))
            (map (lambda (f) (absolute-file-name f name))
                 (directory-files name #t))))

(define (install-thing name-or-pair location . rest)
  (if (active-location? location)
      (let-optionals rest ((target-rel-dir ".") (perms-fn default-perms-fn))
        (install-thing% (fluid *install-layout*)
                        name-or-pair
                        location
                        target-rel-dir
                        perms-fn))))

(define (install-things names-or-pairs . rest)
  (for-each (lambda (name-or-pair)
              (apply install-thing name-or-pair rest))
            names-or-pairs))

(define install-file install-thing)
(define install-files install-things)
(define install-directory install-thing)
(define install-directories install-things)

(define (install-directory-contents name location . rest)
  (if (active-location? location)
      (let-optionals rest ((target-rel-dir ".") (perms-fn default-perms-fn))
        (install-directory-contents% (fluid *install-layout*)
                                     name
                                     location
                                     target-rel-dir
                                     perms-fn))))

(define (install-string% layout str target-name location target-rel-dir)
  (let* ((target-dir (absolute-file-name target-rel-dir
                                         (layout-dir layout location)))
         (target-full-name (absolute-file-name target-name target-dir)))
    (create-directory&parents target-dir)
    (delete-file-or-fail target-full-name)
    (call-with-output-file target-full-name
      (lambda (port) (write-string str port)))))

(define (install-string str target-name location . rest)
  (let-optionals rest ((target-rel-dir "."))
    (if (active-location? location)
        (install-string% (fluid *install-layout*)
                         str
                         target-name
                         location
                         target-rel-dir))))

(define *layout* (make-fluid #f))
(define *install-layout* (make-fluid #f))

;; Return the directory identified by LOCATION in the current layout.
;; If INSTALL? is true, return the directory valid during the
;; installation of the package, otherwise return the directory valid
;; after installation (i.e. during package use).
(define (get-directory location install?)
  (layout-dir (fluid (if install? *install-layout* *layout*)) location))

;; Perform all actions to install PKG in INSTALL-LAYOUT. If LAYOUT is
;; not the same as INSTALL-LAYOUT, assume that some external tool will
;; move the installed files so that they are laid out according to
;; LAYOUT.
(define (install-package layout install-layout pkg)
  (with-cwd (package-directory pkg)
    (let-fluids *layout* layout
                *install-layout* install-layout
                (package-install-thunk pkg))))

;; Install all PACKAGES with the given OPTIONS-VALUES.
(define (install-packages packages options-values)
  (let* ((prefix (alist-get 'prefix options-values))
         (dest-dir (alist-get 'dest-dir options-values))
         (dest-prefix (and prefix (re-root-file-name prefix dest-dir)))
         (layout-fn (resolve-layout (alist-get 'layout options-values)))
         (layout-to (alist-get 'layout-to options-values))
         (build (alist-get 'build options-values))
         (non-shared-only? (alist-get 'non-shared-only options-values))
         (activate? (not (alist-get 'inactive options-values))))
    (let-fluids *options-values* options-values
      (lambda ()
        (for-each
         (lambda (pkg)
           (let* ((rel-layout (layout-fn build pkg))
                  (layout (absolute-layout rel-layout prefix))
                  (i-layout (absolute-layout rel-layout dest-prefix)))
             (if layout-to
                 (call-with-output-file
                     (string-append layout-to "_" (package-full-name pkg))
                   (lambda (port)
                     (write rel-layout port) (newline port))))
             (install-package layout i-layout pkg)
             (if (and activate? (not non-shared-only?))
                 (activate-package i-layout pkg))))
         packages)))))

(define (install-sub-package dir . rest)
  (let-optionals rest ((options-diff '()))
    (with-cwd dir
      (install-packages
       (load-packages)
       (fold (lambda (diff options)
               (cond ((pair? diff)
                      (cons diff (alist-delete (car diff) options)))
                     ((symbol? diff)
                      (alist-delete diff options))
                     (else
                      (error "invalid option difference" diff))))
             (fluid *options-values*)
             options-diff)))))

;;
;; Error handling
;;

;; Display all the MSGS on the error port, then exit with an error
;; code of 1.
(define (display-error-and-exit . msgs)
  (for-each display (cons "Error: " msgs))
  (newline)
  (exit 1))

(define usage #<<END
Usage: ~a [options]

options:
  -h, --help           display this help message, then exit
  --prefix <dir>       specify directory where files are installed
  --layout <layout>    specify layout of installation directory
                       (predefined: ~a)
  --dry-run            don't do anything, print what would have been done
  --verbose            print messages about what is being done
  --inactive           don't activate package after installing it
  --non-shared-only    only install platform-dependent files, if any
  --force              overwrite existing files during installation

advanced options:
  --build <name>          name of platform for which to build
  --layout-from <file>    load layout of installation directory from file
  --layout-to <file>      output layout to given file
  --install-prefix <dir>  specify prefix to used during installation
                          (to be used only during staged installations)

END
)

(define usage-descr-col 26)

;; Complete the above USAGE string to include information about the
;; package options PKG-OPTS.
(define (complete-usage! pkg-opts)
  (let ((usage-port (make-string-output-port)))
    (write-string usage usage-port)
    (write-string "\npackage-specific options:\n" usage-port)
    (for-each
     (lambda (pkg-opt)
       (let ((option/arg (format #f "--~a ~a"
                                 (pkg-opt-name pkg-opt)
                                 (pkg-opt-arg-help pkg-opt))))
         (format usage-port
                 "  ~a~a~a [~a]\n"
                 option/arg
                 (spaces 2 (- usage-descr-col (string-length option/arg)))
                 (pkg-opt-help pkg-opt)
                 ((pkg-opt-show pkg-opt) (pkg-opt-default pkg-opt)))))
     pkg-opts)
    (set! usage (string-output-port-output usage-port))))

;; Display the usage string, then all MSGS on the standard output
;; port, then exit with an error code of 1.
(define (display-usage-and-exit . msgs)
  (format #t
          usage
          (car (command-line))
          (string-join (map car predefined-layouts) ", "))
  (for-each display msgs)
  (newline)
  (exit 1))

;;
;; Command line parsing
;;

;; Predefined parsers/unparsers
(define (parse-boolean s)
  (cond ((string=? s "yes") #t)
        ((string=? s "no") #f)
        (else (display-error-and-exit
               "unknown boolean value '"s"'. Use 'yes' or 'no'."))))

(define (show-boolean b)
  (if b "yes" "no"))

;; The identity function, sometimes useful for parsers/unparsers.
(define (identity x) x)

;; Fluid containing the value of all options.
(define *options-values* (make-fluid #f))

(define package-definition-file "pkg-def.scm")

(define (get-option-value key)
  (alist-get key (fluid *options-values*)))

(define options
  (let ((alist-arg-updater (lambda (key)
                             (lambda (opt name arg alist)
                               (alist-replace key arg alist))))
        (alist-boolean-updater (lambda (key)
                                 (lambda (opt name arg alist)
                                   (alist-replace key #t alist)))))
    (list
     (option '(#\h "help") #f #f
             (lambda args (display-usage-and-exit)))
     (option '("prefix") #t #f (alist-arg-updater 'prefix))
     (option '("dest-dir") #t #f (alist-arg-updater 'dest-dir))
     (option '("layout") #t #f (alist-arg-updater 'layout))
     (option '("layout-from") #t #f
             (lambda (opt name arg alist)
               (alist-replace 'layout
                              (let ((layout (call-with-input-file arg read)))
                                (lambda args layout))
                              alist)))
     (option '("layout-to") #t #f (alist-arg-updater 'layout-to))
     (option '("build") #t #f (alist-arg-updater 'build))
     (option '("non-shared-only") #f #f
             (alist-boolean-updater 'non-shared-only))
     (option '("inactive") #f #f (alist-boolean-updater 'inactive))
     (option '("dry-run") #f #f (alist-boolean-updater 'dry-run))
     (option '("verbose") #f #f (alist-boolean-updater 'verbose))
     (option '("force") #f #f (alist-boolean-updater 'force)))))

(define no-user-defaults-option "--no-user-defaults")

(define (parse-options args options defaults)
  (args-fold args
             options
             (lambda (option name . rest)
               (display-usage-and-exit "Unknown option "name))
             (lambda (operand . rest)
               (display-usage-and-exit "Don't know what to do with " operand))
             defaults))

;; Return user-specific defaults.
(define (read-user-defaults)
  (let ((file (expand-file-name "~/.scsh-pkg-defaults.scm")))
    (if (file-exists? file)
        (call-with-input-file file
          (lambda (port)
            (let ((defaults (read port)))
              (if (or (eof-object? defaults))
                  (display-error-and-exit "no valid defaults found in "file))
              (if (not (eof-object? (read port)))
                  (display-error-and-exit
                   "more than one expression found in "file))
              (eval (list 'quasiquote defaults) (interaction-environment)))))
        '())))

(define options-defaults
  `((prefix          . #f)
    (dest-dir        . "/")
    (layout          . "scsh")
    (layout-to       . #f)
    (build           . ,(host))
    (non-shared-only . #f)
    (inactive        . #f)
    (dry-run         . #f)
    (verbose         . #f)
    (force           . #f)
    (exclude         . ,(lambda args #f))))

(define (install-main cmd-line)
  (if (not (file-exists? package-definition-file))
      (display-error-and-exit "cannot find package definition file"
                              "("package-definition-file")"))
  (let* ((packages (load-packages))
         (all-pkg-opts (all-package-options packages)))
    (if (not (null? all-pkg-opts))
        (complete-usage! all-pkg-opts))
    (let* ((all-opts (append options (map pkg-opt->option all-pkg-opts)))
           (all-dfts (append (alist-combine
                              (if (member no-user-defaults-option cmd-line)
                                  '()
                                  (read-user-defaults))
                              options-defaults)
                             (map pkg-opt-key&default all-pkg-opts)))
           (options-values (parse-options (delete no-user-defaults-option
                                                  (cdr cmd-line))
                                          all-opts
                                          all-dfts))
           (prefix (alist-get 'prefix options-values))
           (layout (alist-get 'layout options-values)))
      (if (not prefix)
          (display-error-and-exit "no prefix specified (use --prefix option)"))
      (if (not (file-name-absolute? prefix))
          (display-error-and-exit "prefix must be an absolute path"))
      (if (not (resolve-layout layout))
          (display-error-and-exit "invalid layout "layout))
      (install-packages packages options-values))))
