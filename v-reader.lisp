;;;; v-reader.lisp

(in-package #:org.unaen.cl.lexer)

(defun |#V-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((func-exp (read stream t nil t)))
    `(multiple-value-call #',(car func-exp) ,@(cdr func-exp))))

(set-dispatch-macro-character #\# #\V #'|#V-reader|)
