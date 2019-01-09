;;;; sets-and-maps.lisp

;(in-package #:org.unaen.cl.lexer)

#| ---------- zset ----------------------------------------------------------- |#
(defclass zset ()
  ((stor :initform (make-hash-table :test 'equal)
         :documentation "Internal storage for the set."))
  (:documentation "My own dumb set implementation."))

(declaim (inline zset-new))
(defun zset-new ()
  "Create a new empty zset object."
  (make-instance 'zset))

(defgeneric zset-add (zset-instance &rest objects)
  (:documentation "Add object to zset; Make object a member of a zset.")
  (:method ((zset-inst zset) &rest objects)
    (with-slots (stor) zset-inst
        (dolist (obj objects)
          (setf (gethash obj stor) t))
        (values zset-inst objects))))

#| ---------- zmap ----------------------------------------------------------- |#
(defclass zmap ()
  ((stor :initform (make-hash-table :test 'equal) ;Initialize first (or default dimension).
         :documentation "Internal storage for the map.")
   (dim :initarg :dimension
        :initform 1
        :reader dimension
        :documentation "The number of input objects that are going to be mapped to the single output object."))
  (:documentation "My own dumb multi-dimensional map implementation."))

(declaim (inline zmap-new))
(defun zmap-new (&key (dimension 1))
  "Create a new empty map object."
  (declare (type integer dimension))
  (make-instance 'zmap
                 :dimension dimension))

(defgeneric zmap-add (zmap-instance from-objects-list to-object)
  (:documentation "Add a mapping from a tuple of elements to a single element in the form of a lisp list of objects to a single lisp object."))

(defmethod zmap-add ((zmap-inst zmap) from-object to-object)
  (zmap-add zmap-inst
            (list from-object)
            to-object))

(defmethod zmap-add ((zmap-inst zmap) (from-objects cons) to-object)
  (labels ((zmap-add-rec (nth-dimension from-objects-remaining)
             (let ((nth-elt (first from-objects-remaining))
                   (nth+1-elt (second from-objects-remaining)))
               (if nth+1-elt                                             ;IF there is a next object in the list:
                   (let ((nth+1-dimension (gethash nth-elt                   ;THEN set in current hash-table for key object to reference the next key object hash table;
                                                   nth-dimension)))
                     (if (null nth+1-dimension)                                  ;IF that next key object hash table doesn't exist:
                         (zmap-add-rec (setf (gethash nth-elt                        ;THEN make a hash-table for it and recurse starting at the next object in the list.
                                                      nth-dimension)
                                             (make-hash-table :test 'equal))
                                       (rest from-objects-remaining))
                         (zmap-add-rec nth+1-dimension                               ;ELSE simply recurse with next key object and hash-table.
                                       (rest from-objects-remaining))))
                   (setf (gethash nth-elt                                    ;ELSE we are on the last key object and so finally set the output to-object.
                                  nth-dimension)
                         to-object)))))
           (if (/= (list-length from-objects)
                   (dimension zmap-inst))
               (error "Incorrect number of from-objects.")
               (zmap-add-rec (slot-value zmap-inst
                                       'stor)
                             from-objects))))
