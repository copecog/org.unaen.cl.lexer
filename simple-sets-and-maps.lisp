;;;; simple-sets-and-maps.lisp

;(in-package #:org.unaen.cl.lexer)

#| ---------- s-set ----------------------------------------------------------- |#
(defclass s-set ()
  ((stor :initform (make-hash-table :test 'equal)
         :accessor stor
         :documentation "Internal storage for the set."))
  (:documentation "My own dumb set implementation."))

(defmacro s-set-new (&rest make-inst-args)
  `(make-instance 's-set ,@make-inst-args))

(defgeneric s-set-add (object/objects-list s-set-instance)
  (:documentation "Add object to s-set; Make object a member of a s-set."))

(defmethod s-set-add (object (s-set-inst s-set))
  (s-set-add-2 object
               s-set-inst))

(defmethod s-set-add ((objects cons) (s-set-inst s-set))
  (values (let ((return-objects (list)))
            (dolist (obj objects)
              (push (s-set-add-2 obj
                                 s-set-inst)
                    return-objects))
            (nreverse return-objects))
          s-set-inst))
              
(declaim (inline s-set-add-2))
(defun s-set-add-2 (object s-set-inst)
  (values (when (setf (gethash object
                               (stor s-set-inst))
                      t)
            object)
          s-set-inst))

#| ---------- s-map ----------------------------------------------------------- |#
(defclass s-map ()
  ((stor :initform (make-hash-table :test 'equal) ;Initialize first (or default dimension).
         :accessor stor
         :documentation "Internal storage for the map.")
   (dim :initarg :dimension
        :initform 1
        :reader dimension
        :documentation "The number of input objects that are going to be mapped to the single output object."))
  (:documentation "My own dumb multi-dimensional map implementation."))

(defmacro s-map-new (&rest make-inst-args)
  `(make-instance 's-map ,@make-inst-args))

(defgeneric s-map-add (to-object from-object/objects-list s-map-instance)
  (:documentation "Add a mapping from a tuple of elements to a single element in the form of a lisp list of objects to a single lisp object."))

(defmethod s-map-add :before (to-object from-obj/s (s-map-inst s-map))
  (if (/= (typecase from-obj/s
            (list (list-length from-objects))
            (atom 1))
          (dimension s-map-inst))
      (error "Incorrect number of from-objects.")
      (call-next-method)))

(defmethod s-map-add (to-object from-object (s-map-inst s-map))
  (let ((from-objects (list from-object)))
    (s-map-add-2 to-object
                 from-objects
                 s-map-inst
                 (stor s-map-inst)
                 from-objects))))

(defmethod s-map-add (to-object (from-objects cons) (s-map-inst s-map))
  (s-map-add-2 to-object
               from-objects
               s-map-inst
               (stor s-map-inst)
               from-objects)))
       
#|
(defmethod s-map-add (to-object (from-objects cons) (s-map-inst s-map))
  (labels ((s-map-add-rec (nth-dimension from-objects-remaining)
             (let ((nth-elt (first from-objects-remaining))
                   (nth+1-elt (second from-objects-remaining)))
               (if nth+1-elt
                   (let ((nth+1-dimension (gethash nth-elt
                                                   nth-dimension)))
                     (if (null nth+1-dimension)
                         (s-map-add-rec (setf (gethash nth-elt
                                                       nth-dimension)
                                              (make-hash-table :test 'equal))
                                        (rest from-objects-remaining))
                         (s-map-add-rec nth+1-dimension
                                        (rest from-objects-remaining))))
                   (setf (gethash nth-elt
                                  nth-dimension)
                         to-object)))))
    (values (if (/= (list-length from-objects)
                    (dimension s-map-inst))
                (error "Incorrect number of from-objects.")
                (s-map-add-rec (stor s-map-inst)
                               from-objects))
            from-objects
            s-map-inst)))
|#

(declaim (inline s-map-dimension-check))
(defun s-map-dimension-check (from-objects s-map-inst)
  (if (/= (list-length from-objects)
          (dimension s-map-inst))
      (error "Incorrect number of from-objects.")))

(declaim (inline s-map-add-2))
(defun s-map-add-2 (to-object from-objects s-map-inst nth-dimension from-objects-remaining)
  (declare (notinline s-map-add-2))
  (let ((nth-elt (first from-objects-remaining))
        (nth+1-elt (second from-objects-remaining)))
    (if nth+1-elt
        (let ((nth+1-dimension (gethash nth-elt
                                        nth-dimension)))
          (if (null nth+1-dimension)
              (s-map-add-2 to-object
                           from-objects
                           s-map-inst
                           (setf (gethash nth-elt
                                          nth-dimension)
                                 (make-hash-table :test 'equal))
                           (rest from-objects-remaining))
              (s-map-add-2 to-object
                           from-objects
                           s-map-inst
                           nth+1-dimension
                           (rest from-objects-remaining))))
        (values (setf (gethash nth-elt
                               nth-dimension)
                      to-object)
                from-objects
                s-map-inst))))

(defgeneric s-map-get (from-object/objects-list s-map-instance)
  (:documentation "Get the output object mapping for a respective object or objects list."))


