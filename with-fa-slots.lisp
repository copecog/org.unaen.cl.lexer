;;;; with-fa-slots.lisp

(in-package #:org.unaen.cl.lexer)

;; state-names -> preface iterate
;; FA          -> regex-tree FA-prev Q-map Q Σ Σ-in-use Δ q₀ q0-prev F dsn
(defmacro with-FA-slots (FA-inst &rest body)
  `(with-slots ((,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".regex-tree")))
		  regex-tree)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".FA-prev")))
		  FA-prev)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Q-map")))
		 Q-map)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Q")))
		 Q)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Σ")))
		 Σ)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Σ-in-use")))
		 Σ-in-use)
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
						      ".q0-prev")))
		 q₀-prev)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".F")))
		 F)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".dsn")))
		 dsn))
       ,FA-inst
     ,@body))
