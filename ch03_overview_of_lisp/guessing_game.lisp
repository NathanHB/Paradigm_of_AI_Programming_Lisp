
(defstruct node
  name
  (true nil)
  (false nil))

(defvar *db*
  "a binary tree where each node")

(setf *db*
  (make-node :name 'object :true
			 (make-node :name 'computer)
			 :false
			 (make-node :name 'vegetable)))

(defun out-of-guess ()
  (format t "~&Im out of guess... What was it?~%~%")
	(make-node :name (read)))

(defun question (&optional (node *db*))
  (format t "~&Is this IT: ~a ~&YES (good category)~&NO (wroong category)~&IT (this is it)~%~%" (node-name node))
  (case (read)
	((yes) (if (not (null (node-true node)))
			   (question (node-true node))
			   (setf (node-true node) (out-of-guess))))
	((no) (if (not (null (node-false node)))
			  (question (node-false node))
			  (setf (node-false node) (out-of-guess))))
	((it) (format t "~&Oh shit found it!~%"))
	(otherwise (progn
		   (format t "~&Wrong input format. Try again~%~%")
		   (question)))))

(defun save-db ()
  "save the database into a file to retrieve it later"
  (with-open-file (stream "tree.db" :direction :output :if-exists :overwrite)
	(print *db* stream)))

(defun load-db (path)
  "load the database from the file given in path"
  (with-open-file (stream path :direction :input)
	(setf *db* (read stream))))

(question)

(save-db)
*db*
(setf *db* nil)
(load-db "tree.db")

