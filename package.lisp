;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :allegretto-regexp
  (:use)
  (:nicknames :aregexp)
  (:export
   :parse-re 
   :split-re 
   :re-submatch 
   :match-re 
   :replace-re 
   :vm 
   :compile-re 
   :native 
   :re-let 
   :re-case 
   :re-lambda 
   :quote-re ))

(defpackage :allegretto-regexp.internal
  (:use :allegretto-regexp :cl :fiveam
        :ppcre)
  (:import-from :ppcre 
                :with-unique-names
                :nsubseq))

