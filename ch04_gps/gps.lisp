;;; ==============================

(defvar *dbg-ids* nil
  "Ids used by dbg")

(defun dbg (id format-string &rest args)
  "if id is in *dbg-ids* then debug will print the string formated with the rest of the args"
  (when (member id *dbg-ids*)
	(fresh-line *debug-io*)
	(apply #'format *debug-io* format-string args)))

(defun dbg-indent (id indent format-string &rest args)
  "if id is in *dbg-ids* then debug-indent will print the string formated with the rest of the args"
  (when (member id *dbg-ids*)
	(fresh-line *debug-io*)
	(dotimes (i indent)
	  (format *debug-io* " "))
	(apply #'format *debug-io* format-string args)))

(defun debug-perso (&rest ids)
  "ads an id to *dbg-ids*"
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug-perso (&rest ids)
  "remove ids from *dbg-ids* if no arguments given remove everything"
  (setf *dbg-ids*
		(if (null ids)
			nil
			(set-difference *dbg-ids* ids))))

(defun find-all (item sequence &rest keyword-args
					  &key (test #'=) test-not &allow-other-keys)
  "return the sequence containing all element meeting the predicate defined with test
  To do so we remove the ones that do not meet the predicate hence the use of complement function"
  (if test-not
	  (apply #'remove item sequence
			 :test-not (complement test-not) keyword-args)
	  (apply #'remove item sequence
			 :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)


(defun start-with (elmt x)
  "return True if x is a list that starts with elmt"
  (and (consp x) (eql (first x) elmt)))

;;; ==============================

(defstruct operator
  action
  (preconditions nil)
  (add-list nil)
  (remove-list nil))

(defun executing-p (x)
  "return True if x starts with executing"
  (start-with 'executing x))

(defun convert-op (op)
  "if op is not of the form executing then it will convert it by adding (executing ?name_of_op) to his add-list"
  (unless (some #'executing-p (operator-add-list op))
	(push (list 'executing (operator-action op)) (operator-add-list op)))
  op)

(defun op (action &key add-list remove-list preconditions)
  "To make an op we make a normal operator and then convert it to executing form to meet our needs"
  (convert-op
	(make-operator
	  :action action :preconditions preconditions :add-list add-list :remove-list remove-list)))


;;; ==============================

(defun action-p (element)
  (or (executing-p element)
	  (equal element '(start)))

(defun apropriate-p (goal op)
  "an op is apropriate to fullfil a goal if it adds the goal to the current
  state that mean if the goal is in its add-list"
  (member-equal goal (operator-add-list op)))

(defun apply-op (state goal op goal-stack)
  "To appply an operation we must achieve all of the prerequisites and then if it succeded
  we append and remove the the content of add-list and remove-list of the op from the current-state variable
  if we cant apply the op return nil"
  (dbg-indent :gps (length goal-stack) "Considering: ~a" (operator-action op))
  (let ((tmp-state (achieve-all state (operator-preconditions op) (cons goal goal-stack))))
	(unless (null tmp-state)
	  (dbg-indent :gps (length goal-stack) "Action: ~a" (operator-action op))
	  (append (remove-if #'(lambda (item) (member-equal item (operator-remove-list op)))
						 tmp-state)
			  (operator-add-list op)))))

(defun member-equal (item list)
  "use the memeber function with the equal predicate to be more flexible"
  (member item list :test #'equal))

(defun achieve (state goal goal-stack)
  "if the goal is already on the goal stack then we are biting our tail -> we cant continue return nil
  if the goal is already in the state -> it is achieve return the state
  otherwise we get all apropriate operator and apply one -> return the result of apply-op"
  (dbg :gps "Goal: ~a" goal)
  (cond ((member goal goal-stack) nil)
		((member goal state) state)
		(t (some #'(lambda (op) (apply-op state goal op goal-stack))
				 (find-all goal *ops* :test #'apropriate-p)))))

(defun achieve-all (state goals goal-stack)
  "tries to achieve all goals by each time setting a new current state and when all goals are achieve
  verifies that they all still stand if its the case then return the state otherwise return nil"
  (let ((current-state state))
	(if (and (every #'(lambda (g)
						(setf current-state (achieve current-state g goal-stack)))
					goals)
			 (subsetp goals current-state :test #'equal))
		current-state)))

(defun gps (state goals *ops*)
  "achieve all goals and remove every atoms to get a list of the executing forms"
  (find-all-if #'action-p (achieve-all (cons '(start) state) goals nil)))

;;; ==============================

(debug-perso :gps)

(gps '(son-at-home car-need-battery have-money have-phone-book)
	 '(son-at-school)
	 *school-ops*)

;;; ==============================

(defparameter *school-ops*
  (list (make-operator :action 'drive-son-to-school
					   :preconditions '(son-at-home car-works)
					   :add-list '(son-at-school)
					   :remove-list '(son-at-home))

		(make-operator :action 'shop-installs-battery
					   :preconditions '(car-need-battery shop-knows-problem shop-has-money)
					   :add-list '(car-works))

		(make-operator :action 'tell-shop-problem
					   :preconditions '(in-communication-with-shop)
					   :add-list '(shop-knows-problem))

		(make-operator :action 'telephone-shop
					   :preconditions '(know-phone-number)
					   :add-list '(in-communication-with-shop))

		(make-operator :action 'look-up-number
					   :preconditions '(have-phone-book)
					   :add-list '(know-phone-number))

		(make-operator :action 'give-shop-money
					   :preconditions '(have-money)
					   :add-list '(shop-has-money)
					   :remove-list '(have-money)))
  "Operations used to drive son to school")

