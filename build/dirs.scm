;;; This file is part of the Scheme Untergrund Library. For copyright
;;; information, see the file COPYING which comes with the
;;; distribution.

(define s48-dirs
  (run/strings (find s48
                     -maxdepth 1
                     -mindepth 1
                     -type d
                     ! -name CVS
                     ! -name rt-modules
                     ! -name sequences)))

(define scsh-dirs
  (run/strings (find scsh
                     -maxdepth 1
                     -mindepth 1
                     -type d
                     ! -name CVS)))
