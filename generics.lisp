(in-package #:org.unaen.src.cl.lexer)

(defgeneric make-state-vector (size &key &allow-other-keys))

(defgeneric make-state-name (state-names))

(defgeneric make-Δ (Σ-type))

(defgeneric push-state (state finite-automaton &key &allow-other-keys))

(defgeneric push-next-states (states-in-tree FA))

(defgeneric push-transit-2 (state-A state-B transit-char FA Σ))

(defgeneric delete-transit (state-A state-B transit-char FA))

(defgeneric get-transit (state transit-char Δ))

(defgeneric ε-closure (state NFA))

(defgeneric get-state (return-value state-property FA))

(defgeneric find-name-equal (thing1 thing2))

(defgeneric get-Δ (state Δ))

(defgeneric get-all-transit (transit-char Δ))

(defgeneric push-state-new (state FA &key &allow-other-keys))

(defgeneric push-fragment-2 (fragment-type specifications-list NFA))

(defgeneric push-fragment (regex-fragment-tree NFA &rest pass-forward-args))

(defgeneric char-interval->list (char-start char-end))

(defgeneric list->pairs (source-list))

(defgeneric regex-tree->nfa (regex-expr-tree))
