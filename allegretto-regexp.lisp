;;;; allegretto-regexp.lisp

(cl:in-package :allegretto-regexp.internal)
;; (in-readtable :allegretto-regexp)


;;; http://www.franz.com/support/documentation/9.0/doc/operators/excl/re-lambda.htm

(defstruct regular-expression-match
  indexes input num-submatches named-submatches)


(defmethod make-load-form ((obj regular-expression-match)
                            &optional env)
  (declare (ignore env))
  `(make-regular-expression-match 
    :indexes ',(regular-expression-match-indexes obj)
    :input ,(regular-expression-match-input obj)
    :num-submatches ,(regular-expression-match-num-submatches obj)
    :named-submatches ',(regular-expression-match-named-submatches obj)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-var-list-re (list)
    ;; (var integer) -- var binds to integer-th submatch (0 for whole).
    ;; (var string/symbol) -- var binds to the submatch named by string/symbol.
    ;; var -- var binds to the submatch named by var.
    (mapcar (lambda (e) 
              (if (consp e)
                  (list (car e)
                        (etypecase (cadr e)
                          (symbol (string (cadr e)))
                          ((or integer string) (cadr e)) ))
                  (list e (string e)) ))
            list ))


  (defun list-flatten (tree)
    (let ((result '()))
      (labels ((*scan (item)
                 (if (consp item)
                     (map nil #'*scan item)
                     (push item result) )))
        (*scan tree) )
      (nreverse result) ))


  (defun regname-pos-alist (regex-tree)
    ;; TODO: まあ安直にflattenして出現した順番ですわ
    (let ((u (list-flatten regex-tree)))
      (do ((e u (cdr e))
           (ans '() )
           (cnt 1) )
          ((endp e) ans)
        (case (car e)
          ((:named-register) 
           (push `(,(cadr e) ,cnt) ans)
           (incf cnt) )
          ((:register :nop)) ))))


  (defun regpos (pos-designator alist)
    (etypecase pos-designator
      (integer pos-designator)
      (string (second (assoc pos-designator alist :test #'string=))) ))
  

  (defun ignore-whitespace-re (re)
    (let ((s (create-scanner "\\s*#.*")))
      (regex-replace-all "\\s"
                         (regex-replace-all s re "") 
                         "")))
  )


(defun match-re (regex target-string 
                       &key 
                       (return :string)
                       (case-fold t)
                       (single-line nil)
                       (multiple-lines nil)
                       (ignore-whitespace nil)
                       (start 0)
                       (end (length target-string)) )
  (declare #.ppcre::*standard-optimize-settings*
           (string target-string) )
  (let ((re (let ((*allow-named-registers* t))
              (parse-string (if ignore-whitespace
                                (ignore-whitespace-re regex)
                                regex)) )))
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
                         (funcall  
                          (the function 
                            (create-scanner re
                                            :multi-line-mode multiple-lines
                                            :single-line-mode single-line
                                            :case-insensitive-mode (not case-fold)))
                          target-string
                          start
                          end )
      (declare (simple-vector reg-starts reg-ends))
      (unless match-start
        (return-from match-re nil) )
      (ecase return
        ((:string)
         (multiple-value-call 
           #'values 
           T
           (subseq target-string match-start match-end)
           (values-list
            (map 'list
                 (lambda (reg-start reg-end)
                   (if reg-start
                       (subseq target-string reg-start reg-end)
                       nil ))
                 reg-starts
                 reg-ends ))))
        ((:index) 
         (multiple-value-call 
           #'values 
           T
           (cons match-start match-end)
           (values-list
            (map 'list
                 (lambda (reg-start reg-end)
                   (if reg-start
                       (cons reg-start reg-end)
                       nil ))
                 reg-starts
                 reg-ends ))))
        ((:match)
         (make-regular-expression-match 
          :indexes (acons match-start match-end
                          (map 'list
                               (lambda (reg-start reg-end)
                                 (if reg-start
                                     (cons reg-start reg-end)
                                     nil ))
                               reg-starts
                               reg-ends ))
          :input target-string
          :num-submatches (length (the vector reg-starts))
          :named-submatches (regname-pos-alist re)))))))


(defmethod re-submatch ((regexp regular-expression-match) string indexes selector
                        &key (type :string))
  (declare (ignore type string indexes))
  (destructuring-bind (s . e)
                      (if (integerp selector)
                          (nth selector (regular-expression-match-indexes regexp))
                          (nth (second 
                                (assoc selector
                                       (regular-expression-match-named-submatches regexp)
                                       :test #'string=))
                               (regular-expression-match-indexes regexp)))
    (subseq (regular-expression-match-input regexp) 
            s e)))


(defmacro re-lambda (regex var-list &body body)
  (with-unique-names (target-string if-does-not-match match)
    `(lambda (,target-string &key ((:if-does-not-match ,if-does-not-match) nil))
       (let ((,match (match-re ,regex ,target-string :return :match)))
         (if ,match
             (let (,@(mapcar (lambda (e) 
                               `(,(first e)
                                  (re-submatch ,match nil nil ,(second e))))
                             (normalize-var-list-re var-list)))
               ,@body)
             ,if-does-not-match)))))


#|(defmacro re-lambda (regex var-list &body body
                     &aux start end sharedp
                       (ppcre:*allow-named-registers* t)
                       (regname.pos (regname-pos-alist (parse-string regex))))
  (with-unique-names (match-start match-end reg-starts reg-ends
                      start-index substr-fn target-string
                      if-does-not-match )
    `(lambda (,target-string
              &key ((:if-does-not-match ,if-does-not-match) NIL))
       (multiple-value-bind (,match-start ,match-end ,reg-starts ,reg-ends)
                            (scan (let ((*allow-named-registers* t))
                                    (create-scanner (parse-string ,regex)))
                                  ,target-string :start (or ,start 0)
                                  :end (or ,end (length ,target-string)))
         (declare (ignorable ,match-end))
         (if ,match-start            
             (let* ,(list*
                     `(,substr-fn (if ,sharedp
                                      #'nsubseq
                                      #'subseq ))
                     (loop :for (var pos) :in (normalize-var-list-re var-list)
                           :when var
                             :collect (cond ((eql 0 pos)
                                             `(,var (funcall ,substr-fn
                                                             ,target-string
                                                             ,match-start
                                                             ,match-end )))
                                            ((setq pos (regpos pos regname.pos))
                                             (let ((pos (1- pos)))
                                               `(,var (let ((,start-index
                                                             (aref ,reg-starts ,pos ) ))
                                                        (if ,start-index
                                                            (funcall ,substr-fn
                                                                     ,target-string
                                                                     ,start-index
                                                                     (aref ,reg-ends ,pos ) )
                                                            nil )))))
                                            (T `(,var nil)) )))
               ,@body )
             ,if-does-not-match )))))|#


(defmacro re-let (regexp string bindings &body body)
  `(funcall (re-lambda ,regexp ,bindings ,@body)
            ,string))


(defmacro re-case (string &body clauses)
  (with-unique-names (s not-match)
    `(let ((,s ,string) 
           (,not-match (cons nil nil)) )
       ,(labels ((*re-case (s clauses)
                   (if (endp clauses)
                       '()
                       (with-unique-names (match-fn match)
                         (destructuring-bind (regex &optional binds &rest body) 
                                             (car clauses)
                           (if (eq t regex)
                               `(progn ,binds ,@body)
                               `(let* ((,match-fn (re-lambda ,regex ,binds ,@body))
                                    (,match (funcall ,match-fn ,s 
                                                     :if-does-not-match ,not-match)) )
                                  (if (not (eq ,not-match ,match))
                                      ,match
                                      ,(*re-case s (cdr clauses)) ))))))))
          (*re-case s clauses)))))


;;; eof


