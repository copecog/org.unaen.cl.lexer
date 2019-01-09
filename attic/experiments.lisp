(in-package #:cl-user)

;;
(defgeneric foo-1 (par-1 &optional par-2 &rest par-rest)
  (:method ((par-1 list) &optional (par-2 'par-2-default) &rest par-rest)
    (apply #'foo
           (first par-1)
           par-2 (rest par-1)))
  (:method ((blah (eql 'blah)) &optional par-2 &rest par-rest)
    (values blah par-2 par-rest)))

(defun foo-2 (var1 &optional (var2 'var2) &rest rest-var)
  (values var1 var2 rest-var))

;;
(defun pass-through (stream subchar arg)
  (let ((stuff (read stream nil (values) t)))
    `(list ':subchar ',subchar ':stuff ',stuff ':arg ',arg)))

;;
(defun |#V-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((func-exp (read stream t nil nil)))
    `(multiple-value-call #',(car func-exp) ,@(cdr func-exp))))

(set-dispatch-macro-character #\# #\V #'|#V-reader|)

;;
(defun happy-new-year ()
  (when (= 2019 (nth-value 5 (decode-universal-time (get-universal-time)))) (princ "Happy new year!")))

;;
(defclass test-class-1 ()
  ((test-slot-1 :initarg :test-slot-1
                :initform nil
                :accessor test-slot-1)
   (test-slot-2 :initarg :test-slot-2
                :initform nil
                :accessor test-slot-2)))
