(+ 2 2)
(append '(Max sandy) (list '(John Rick) 'Charles))
(setf p '(john q smith))
pi


(defun remove-last (l)
  (reverse (rest (reverse l))))

(defun last-name (name)
  "Select the lastname from a name represented as a list"
  (if (member (first (last name)) *titles*)
	  (last-name (remove-last name))
	  (first (last name))))

(defun first-name (name)
  "Select the firstname from a name represented as a list"
  (if (member (first name) *titles*)
	  (first-name (rest name))
	  (first name)))

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General Monsieur Jr MD)
  "List of titles that can appear at start of name")


(setf names '((john Q public)
			 (Malcom X)
			 (Admiral Grayce Murray Hopper)
			 (spot)
			 (Aristotle)
			 (A A Milne)
			 (Z Z Top)
			 (Sir Larry Olivier)
			 (Miss Scarlet)
			 (Rex Morgan MD)
			 (Morton Downey Jr)))

(last-name '(Nathan F Habib))
(first-name '(Nathan F Habib))

(mapcar #'last-name names)
(mapcar #'first-name names)

(trace last-name)
(last-name '(Madam Major General Paula Jones MD))
(last-name '(Madam Major General Paula Jones MD Jr Dr))
(untrace last-name)

(defun mappend (fn list)
  "Apply fn to every atom in list and append the results"
  (apply #'append (mapcar fn list)))

(defun self-and-double (x)
  (list x (* x 2)))

(self-and-double 3)
(mapcar #'self-and-double '(1 10 300))
(mappend #'self-and-double '(1 10 300))

(defun number-and-negation (x)
  "if x is a number return x and hhis negation"
  (if (numberp x)
	  (list x (- x))
	  nil))

(defun numbers-and-negation (l)
  "Given a list return only nukbers and their negation"
  (mappend #'number-and-negation l))

(numbers-and-negation '(test 1 2 3))

(mappend #'(lambda (l) (list l (reverse l))) '((1 2 3) (a b c)))

(defun power (x y)
  "returns x**y"
  (if (<= y 0)
	  1
	  (* x (power x (1- y)))))

(defun count-atoms (l)
  "returns the number of atoms in l a list"
  (if (not l)
	  0
	  (+ 1 (count-atoms (rest l)))))

(defun count-anywhere (expr l)
  "returns the number of times expr appears in l a list"
  (cond ((eql expr l) 1)
		((atom l) 0)
		(t (+ (count-anywhere expr (first l)) (count-anywhere  expr (rest l))))))

(defun dot-product (l1 l2)
  "computes the dot product of two vector l1 and l2"
  (apply #'+ (mapcar #'* l1 l2)))
