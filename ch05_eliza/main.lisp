; ===========

;; ABSCRACTIONS

(defconstant fail nil "indicate pattern-matching failure")

(defconstant no-bindings '((T . T))
  "Indicate pattern-matching succces with no variables")

(defun get-binding (var bindings)
  "just abscrat assoc away; find (var . value) pair in bindings"
  (assoc var bindings))

(defun binding-val (binding)
  "get value part of a binding pair"
  (cdr binding))

(defun lookup (var bindings)
  "returns the value corresponding to the key var in the bindings"
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Adds the (var . val) binding to bindings and remove the no-bindings mark"
  (cons (cons var val) (if (equal '((T . T)) bindings) nil bindings)))

; ==========


(defun find-binding (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
	(cond ((not binding) (extend-bindings var input bindings))
		  ((equal input (binding-val binding)) bindings)
		  (t fail))))

(defun variable-p (x)
  "Test wether or not a x is a variable
  By convention a variable starts with ?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun segment-variable-p (x)
  "Test wether or not x is a segment variable
  By convention, a segment variable has the form ((?* variable) . pat)"
  (and (consp x)
	   (equal (first (first x)) '?*)
	   (variable-p (second (first x)))))

(defun match-segment-variable (pattern input bindings)
  "pattern is of the form ((?* var) . pat)"
  (let ((pat (second pattern)))
	(if (null pat)
		(find-binding (second (first pattern)) input bindings)
		(let ((pos (position pat input :test #'equal)))
		  (if (null pos)
			  fail ; not finished
			  ())))

(defun pattern-match (pattern input &optional (bindings no-bindings))
  "Tests an input against a pattern"
  (cond ((eq fail bindings) fail)
		((variable-p pattern)
		 (find-binding pattern input bindings))
		((eql pattern input) bindings)
		((segment-variable-p pattern)
		 (match-segment-variable pattern input bindings))
		((and (consp pattern) (consp input))
		 (pattern-match (rest pattern) (rest input)
						(pattern-match (first pattern) (first input) bindings)))
		(t fail)))

(trace pattern-match)
(trace find-binding)
(segment-variable-p '(?* ?n))
(pattern-match '(?p like . ?x) '(I like chocolate candy))
