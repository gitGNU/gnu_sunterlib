;;; Installation library for scsh modules.
;;; $Id: install-lib-module.scm,v 1.1 2004/03/11 19:01:40 acarrico Exp $

;;; Interfaces

(define-interface install-interface
  (export tmpl-libtool-la-reader

          version->string
          string->version
          version-compare
          version<?
          version>?
          version=?

          (define-package :syntax)
          load-package-in

          install-file
          install-files
          install-directory
          install-directories
          install-directory-contents
          install-string
          install-sub-package

          identity
          parse-boolean
          show-boolean

          get-directory
          get-option-value
          with-output-to-load-script*
          (with-output-to-load-script :syntax)
          write-to-load-script

          install-main))

;;; Structures

(define-structure install install-interface
  (open scheme-with-scsh
        cells
        fluids
        let-opt
        srfi-1
        srfi-9
        srfi-13
        srfi-37
        configure
        pp)
  (files install-lib))
