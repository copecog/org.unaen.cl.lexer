;;;; org.unaen.cl.lexer/util.lisp

(in-package #:org.unaen.cl.lexer)

(defmacro with-FA-slots (FA-inst &rest body)
  `(with-slots ((Q Q) (Σ Σ) (Δ Δ) (q₀ q₀) (F F)) ,FA-inst
     ,@body))

(defmacro with-FA-dot-slots (FA-inst &rest body)
  `(with-slots ((,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Q")))
		 Q)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Σ")))
		 Σ)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Δ")))
		 Δ)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".q0")))
		 q₀)

		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".F")))
		 F))
       ,FA-inst
     ,@body))

(defmacro with-FA-system-slots (FA-system-inst &rest body)
  `(with-slots ((reglex reglex) (FA FA) (FA-prev FA-prev) (Q-maps Q-maps) (state-kernel state-kernel)) ,FA-system-inst
     ,@body))

(defmacro with-FA-system-dot-slots (FA-inst &rest body)
  `(with-slots ((,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".reglex")))
		 reglex)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".FA")))
		 FA)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".FA-prev")))
		 FA-prev)	
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Q-maps")))
		 Q-maps)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".state-kernel")))
		 state-kernel))
       ,FA-inst
     ,@body))


