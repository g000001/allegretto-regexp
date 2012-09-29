;;;; allegretto-regexp.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :allegretto-regexp
  :serial t
  :depends-on (:fiveam
               :cl-ppcre)
  :components ((:file "package")               
               (:file "allegretto-regexp")
               (:file "test")))


(defmethod perform ((o test-op) (c (eql (find-system :allegretto-regexp))))
  (load-system :allegretto-regexp)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :allegretto-regexp.internal :allegretto-regexp))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

