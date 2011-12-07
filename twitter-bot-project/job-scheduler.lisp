(in-package :cl-twitter-projects-twitter-bot-project)

(defvar *scheduled-job-table*  (make-hash-table :test 'equalp) "table of scheduled jobs")

(defun mongodb-error-handler (id &key (db "scheduler") (collection "error") (mongo nil))
    (lambda (condition)
      (progn
	(with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db db )
	  (cl-mongo:db.insert collection (cl-mongo:kv (cl-mongo:kv "id" id) 
						      (cl-mongo:kv "timestamp" (cl-mongo::make-bson-time))
						      (cl-mongo:kv "condition" (format nil "~A" condition))) :mongo mongo)))))

;;(print-object (car (get-element "timestamp" (docs (db.find "error" :all :selector "timestamp")))) nil)

(defun fmt-ts (bson-time)
  (print-object bson-time nil))

(defun show-errors ( &key (db "scheduler") (collection "error"))
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db db)
    (let* ((docs* (docs (iter (db.find collection :all))))
	   (elements (cl-mongo:collect-all-elements (list "id" "condition" "timestamp") docs*)))
      (dolist (el elements)
	(format t "~A | ~A | ~A ~%" (nth 0 el) (nth 1 el) (fmt-ts (nth 2 el)))))))

(defun make-scheduled-job (func &key (args nil) (every 0) (iter 100) (maxerror 5) (errorhandler #'identity))
  (progn
    (unless (null args) (assert (equal (type-of args) 'cons)))
    (lambda ()
      (loop
	 (progn
	   (handler-case
	       (apply func args)
	     (error (condition)
	       (funcall errorhandler condition)
	       (decf maxerror)))
	   (when (< 0 iter) (decf iter))
	   (when (>= 0 every) (return 'done))
	   (when (= 0 iter)   (return 'iter-done))
	   (when (= 0 maxerror)  (return 'error-done))
	   (sleep every))))))

(defun submit-job (name func &key (args nil) (every 0) (iter 100) (errorhandler nil) (maxerror 5))
  (let* ((errhandle (and errorhandler (mongodb-error-handler name)))
	 (thread (bordeaux-threads:make-thread  (make-scheduled-job func :args args :every every :iter iter :errorhandler errhandle :maxerror maxerror) :name name)))
    (setf (gethash name *scheduled-job-table*) thread))) 


(defun show-job (name)
  (multiple-value-bind (thread exists-p) (gethash name *scheduled-job-table*)
    (if exists-p
	thread
	(format nil "job with name ~A not found ~%" name))))

(defun map-all-jobs (fn)
  (with-hash-table-iterator (iterator *scheduled-job-table*)
    (dotimes (repeat (hash-table-count *scheduled-job-table*))
      (multiple-value-bind (exists-p key value) (iterator)
	(if exists-p (funcall fn key value))))))

(defun gc-jobs ()
  (let ((lst))
    (map-all-jobs (lambda (k v) (unless (bordeaux-threads:thread-alive-p v) (push k lst))))
    (dolist (item lst)
      (remhash item *scheduled-job-table*))))

(defun kill-job (name)
  (multiple-value-bind (thread exists-p) (gethash name *scheduled-job-table*)
    (if exists-p
	(bordeaux-threads:destroy-thread  thread)
	(format nil "job with name ~A not found ~%" name))))

(defun show-all-jobs ()
  (map-all-jobs (lambda (k v) (format t "~S:~S~%" k v))))

(defun kill-all-jobs ()
  (map-all-jobs (lambda (name thread) (declare (ignore name)) (bordeaux-threads:destroy-thread  thread))))


(defun test-job (arg)
  (error arg))

(defun run-test-job (arg &key (every 10) (iter 10) )
  (submit-job "test-job" #'test-job :args (list arg) :every every :iter iter :errorhandler t))