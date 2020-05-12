
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
	(noun-phrase -> (article adj* noun pp*) (name) (pronoun))
	(verb-phrase -> (verb noun-phrase))
	(adj* -> () (adj adj*))
	(pp* -> () (pp pp*))
	(article -> the a)
	(adj -> big tall blue red adiabatic)
	(pp -> (prep noun-phrase))
	(prep -> to in on by with)
	(name -> Pat kim lee robin)
	(pronoun -> he she it that)
	(noun -> man ball woman table)
	(verb -> hit took saw liked))
  "A grammar for a trivial subset of english")

(defparameter *math-grammar*
  '((expression -> (variable operation variable))
	(variable -> number letter)
	(number -> non-zeros+ numeric+)
	(non-zeros+ -> (non-zeros) (non-zeros non-zeros+))
	(numeric+ -> (numeric) (numeric numeric+))
	(non-zeros -> 1 2 3 4 5 6 7 8 9)
	(numeric -> 0 non-zeros)
	(letter -> x y z t a b c)
	(operation -> + = - * % /))
  "A grammar for bad maths expressions")

(defvar *grammar* *simple-grammar*
  "The grammar that will be used by #'generate")

(setf *grammar* *math-grammar*)

(defun rule-rhs (rule)
  "Returns the right had side of the rule"
  (rest (rest rule)))

(defun rule-lhs (rule)
  "The left hand side of rule"
  (first rule))

(defun rewrite (category)
  "return list of possible rewrite for the category"
  (rule-rhs (assoc category *grammar*)))

(defun random-elt (l)
  "pick a random element of l"
  (elt l (random (length l))))

(defun is-terminal-p (category)
  "return True if category is non terminal symbol"
  (not (null (rewrite category))))

(defun generate (category)
  "Generate a phrase of the category given"
  (cond
	((listp category)
	 (mappend #'generate category))

	((is-terminal-p category)
	 (generate (random-elt (rewrite category))))

	(t (list category))))

(defun generate-tree (category)
  "Generate a phrase of the category given"
  (cond
	((listp category)
	 (mapcar #'generate-tree category))

	((is-terminal-p category)
	 (cons category
		   (generate-tree (random-elt (rewrite category)))))

	(t (list category))))

(generate 'expression)
(generate-tree 'sentence)
