;;;-----------------------------------------------------------------------------
;;; 
;;; mini-argparse.asd -- asdf system definition for mini-argparse
;;; 
;;; Copyright (C) 2019 Severin Kempf skempf@indyeng.com
;;; 
(in-package #:asdf-user)

(defsystem "mini-argparse"
  :description "Common Lisp argument parser inspired by Python's argparse."
  :author "Severin Kempf severin.kempf@indyeng.com"
  :maintainer "Severin Kempf severin.kempf@indyeng.com"
  :license "ISC"
  :depends-on nil
  :components ((:file "package")
               (:file "mini-argparse")))

;;;-----------------------------------------------------------------------------
;;; End
