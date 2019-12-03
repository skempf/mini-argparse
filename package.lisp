;;;-----------------------------------------------------------------------------
;;; 
;;; package.lisp -- package definition for mini-argparse
;;;
;;; Copyright (C) 2019 Severin Kempf skempf@indyeng.com
;;; 
(in-package #:cl-user)

(defpackage #:mini-argparse
  (:use #:cl)
  (:nicknames #:argparse)
  (:export 
   #:parser #:add-optional #:add-positional #:process-args
   #:print-help-and-exit #:print-error-and-exit))

;;;-----------------------------------------------------------------------------
;;; End
