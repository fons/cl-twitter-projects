(in-package :cl-twitter-projects-twitter-bot-project)


(defun dump-social-graph (screen-name)
  (cl-twitter:use-cache)
  (cl-twitter:use-db :twitter-mongodb-driver)
  (cl-twitter:db-status)
  (collect-follower-ids screen-name)
  (collect-friend-ids screen-name))


(defun filter (pred lst)
  (let ((L ()))
    (dolist (el lst)
      (when (funcall pred el) (push el L)))
    (nreverse L)))

(defun cached-user-id (id)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
    (car (docs (db.find "twitter-user" ($ "_id" id))))))

(defun users-not-cached (ids)
  (filter (lambda (id) (not (cached-user-id id))) ids))

(defun chunk* (lst n accum)
  (if (or (< n 1) (null lst)) (list accum lst)
      (chunk* (cdr lst) (- n 1) (cons (car lst) accum))))

(defun chunk** (lst n accum)
  (let ((res (chunk* lst n ())))
    (if (null (cadr res)) (cons (car res) accum)
	(chunk** (cadr res) n (cons (car res) accum)))))

(defun lst->chunks (lst &key (size 100))
  (chunk** lst size ()))


(defun lookup-users-restart (idstr &key (retry 5))
  (loop
     (decf retry)
     (when (> 0 retry) (return 'error))
       (handler-case 
	   (progn
	     (lookup-users nil :user-id idstr) 
	     (return 'done))
	 (error (e)
	   (format t "[~S] ~S~%" retry e)))))


(defun walk-cached-social-graph/followers (screen-name &key (maxloop 10000) (cursor -1))
  (let ((result ())
	(loopies 0)
	(entities (list "ids" "next-cursor")))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
      (loop 
	 (setf result (car (cl-mongo:collect-all-elements entities (docs (db.find "social-graph-cursor-id" 
										  ($ ($ "screen-name" screen-name) 
										     ($ "command" "FOLLOWERS/IDS") ($ "cursor" cursor)))))))
	 (dolist (chunk (lst->chunks (mapcar (lambda (n) (format nil "~S" n)) (users-not-cached (car  result)))))
	   (when chunk
	     (let ((idstr (reduce (lambda (s tag) (concatenate 'string s "," tag)) chunk )))
	       (format t "looking up chunk ~S~%" idstr)
		 (lookup-users-restart idstr)
	       )))
	 (setf cursor (cadr result))
	 (incf loopies)
	 (format t "next cursor ~S [loop ~S]~%" cursor loopies)
	 (when (or (< cursor 1) (> loopies maxloop)) (return 'done))))))
      

(defun merge-tags (txt tags)
  (reduce (lambda (s tag ) (concatenate 'string s " " tag)) tags :initial-value txt))


(defun make-tweets (docs)
  (let ((pair-lst (cl-mongo:collect-all-elements (list "text"  "tags") docs)))
    (mapcar (lambda (p) (merge-tags (car p) (cadr p))) pair-lst)))

(defun read-quote-ids (dbname collection)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db dbname )
    (let ((id (select-random (cl-mongo:get-element :_id (cl-mongo:docs (db.iter (cl-mongo:db.find collection :all :selector "_id")))))))
      (docs (db.find collection ($ "_id" id) )))))


(defun more-random (n)
  (let ((nxt (random n)))
    (mod (random (* 2 nxt)) n)))

(defun select-random (lst) 
  (nth (more-random (+ 1 (length lst))) lst))


(defun twitter-bot (screen-name)
  (cl-twit-repl:get-authenticated-user screen-name)
  (dolist (tw (make-tweets (read-quote-ids "maozedong" "lrbbase")))
    (cl-twitter:tweet  tw)))

(defun job-twitter-bot (screen-name)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" screen-name))
  (twitter-bot screen-name))

(defun start-job-twitter-bot (screen-name &key (every 3000) (iter 10))
  (submit-job (concatenate 'string "job-twitter-bot-" screen-name) #'job-twitter-bot :args (list screen-name) :every every :iter iter :errorhandler t))
  
