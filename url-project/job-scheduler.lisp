(in-package :cl-twitter-projects-url-project)

(defvar *scheduled-job-table*  (make-hash-table :test 'equalp) "table of scheduled jobs")

(defun mongodb-error-handler (id &key (db "scheduler") (collection "error") (mongo nil))
  (progn
    (cl-mongo:db.use db :mongo mongo)
    (lambda (condition)
      (cl-mongo:db.insert collection (cl-mongo:kv (cl-mongo:kv "id" id) (cl-mongo:kv "condition" (format nil "~A" condition))) :mongo mongo))))

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




