(defstruct name
  first
  (middle nil)
  last)

(defvar b)
(setf b (make-name :first 'Bertrand :last 'Russel))
(name-first b)
(name-last b)
(name-middle b)
(setf (name-middle b) 'Arthur-William)
b

(defvar c '(1 2))

(defun dprint (x)
  "print x in dotted notation"
  (cond ((atom x) (princ x))
		(t (princ "(")
		   (princ (first x))
		   (princ " . ")
		   (dprint (rest x))
		   (princ ")")
		   x)))

(dprint '(1 2 3 (4 5)))

(setf state-table
	  '((AL . alabama) (AK . alaska) (AZ . arizona) (AR . arkansas)))

(assoc 'Ar state-table)
(cdr (assoc 'ar state-table))
(rassoc 'alabama state-table)

(setf table (make-hash-table))
(setf (gethash 'al table) 'alabama)
(gethash 'al table)


(defun problem (x op y)
  (format t "~&What is ~d ~a ~d?" x op y)
  (if (= (read) (funcall op x y))
	  (format t "~&It is correct!~%")
	  (format t "~&Incorect~%")))

(defun math-quizz (&key (op '+) (range 100) (n 1))
  (dotimes (i n)
	(problem (random range) op (random range))))

(math-quizz)

(defun length-m (sequence)
  (reduce #'+ sequence :key #'(lambda (x) 1)))


(print 'hey)
(format t "~@(~{~a~^ ~}.~)" '(This is a sentence))
