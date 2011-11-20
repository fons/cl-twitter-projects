(in-package :cl-twitter-projects-twitter-bot-project)


(defvar *twitter-monogodb-initialized* nil)

;;(defun make-social-graph-unique-ids 

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


(defun cache-social-graph/followers (screen-name &key (maxloop 10000) (cursor -1) (chunk-size 100))
  (let ((result ())
	(loopies 0)
	(entities (list "ids" "next-cursor")))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
      (loop 
	 (setf result (car (cl-mongo:collect-all-elements entities (docs (db.find "social-graph-cursor-id" 
										  ($ ($ "screen-name" screen-name) 
										     ($ "command" "FOLLOWERS/IDS") ($ "cursor" cursor)))))))
	 (when result
	   (dolist (chunk (lst->chunks (mapcar (lambda (n) (format nil "~S" n)) (users-not-cached (car  result))) :size chunk-size))
	     (when chunk
	       (let ((idstr (reduce (lambda (s tag) (concatenate 'string s "," tag)) chunk )))
		 (format t "looking up chunk ~S~%" idstr)
		 (lookup-users-restart idstr)))))
	 (setf cursor (cadr result))
	 (incf loopies)
	 (format t "next cursor ~S [loop ~S]~%" cursor loopies)
	 (when (or (< cursor 1) (> loopies maxloop)) (return 'done))))))

(defun zip* (l r accum)
  (cond ((null l) (nreverse accum))
	((null r) (nreverse accum))
	(t        (zip* (cdr l) (cdr r) (cons (list (car l) (car r)) accum)))))

(defun zip (l r)
  (zip* l r ()))

(defun princ2 (el stream)
  (format stream "~S~%" el))

(defun princ2csv (el stream)
  (format stream "~A~%" (subseq (reduce (lambda (a b) (format nil "~A,~A" a (cadr b))) el :initial-value "") 1)))

(defun graph-data (idlst &key (stream t) (f #'princ2))
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
    (dolist (id idlst)
      (let* ((docs (docs (db.find "twitter-user" ($ "_id" id) :selector ($+ "screen-name" "followers-count" "friends-count" "statuses-count"))))
	     (fields (list :_id "screen-name" "followers-count" "friends-count" "statuses-count"))
	     (kw     (mapcar (lambda (w) (intern (string-upcase w) :keyword)) (cdr fields)))
	     (vallst (collect-all-elements fields docs))
	     (elems  (mapcar (lambda (lst) (zip (cons :_id kw) lst)) vallst))) 
	(dolist (el elems)
	  (funcall f el stream)	  
	  )))))

(defun save-graph-data (screen-name fn &key (f #'identity) (maxloop 100000))
  (with-open-file (stream fn :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "id,screen_name,followers,friends,tweets~%")
    (walk-cached-social-graph/followers screen-name (lambda (idlst) (graph-data idlst :stream stream :f f)) :maxloop maxloop)))

(defun follow-back (screen-name idlst )
  (dolist (id idlst)
    (let* ((id-name  (car (get-element "screen-name"  (docs (db.find "twitter-user" ($ "_id" id) :selector ($+ "screen-name" )))))))
      (handler-case (unless (user-a-following-user-b? screen-name id-name)
		      (progn
			(format t "~A NOT following ~A~%" screen-name id-name)
			  (follow id-name)
			))
	(error (e)
	  (format t "[~S] ~S~%" id-name e))))))


(defun walk-cached-social-graph/followers (screen-name func &key (maxloop 10000) (cursor -1) (chunk-size 100))
  (let ((result ())
	(loopies 1)
	(entities (list "ids" "next-cursor")))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
      (loop 
	 (setf result (car (cl-mongo:collect-all-elements entities (docs (db.find "social-graph-cursor-id" 
										  ($ ($ "screen-name" screen-name) 
										     ($ "command" "FOLLOWERS/IDS") ($ "cursor" cursor)))))))
	 (dolist (chunk (lst->chunks (car  result) :size chunk-size))
	   (funcall func chunk))
	 (setf cursor (cadr result))
	 (format t "next cursor ~S [loop ~S]~%" cursor loopies)
	 (incf loopies)
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
  

;;
;; analysis
;;

;;(defun tweet-follower-friend-analysis ()
;;  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
;;    (pp (db.find "twitter-user" :all :selector ($+ "followers-count" "friends-count" "statuses-count")))))

(defun dump-social-graph (screen-name)
  (cl-twitter:use-cache)
  (cl-twitter:use-db :twitter-mongodb-driver)
  (cl-twitter:db-status)
  (collect-follower-ids screen-name)
  (collect-friend-ids screen-name))

(defun auto-follow-back (screen-name)
  (labels ((fb (id-name)
	     (follow-back screen-name id-name)))
    (unless *twitter-monogodb-initialized* 
      (progn 
	(cl-twitter:use-cache)
	(cl-twitter:use-db :twitter-mongodb-driver)
	(setf *twitter-monogodb-initialized*  'DONE)))
    (rm "social-graph-cursor-id" ($ "screen-name" screen-name))
    (collect-follower-ids screen-name)
    (collect-friend-ids screen-name)
    (cache-social-graph/followers screen-name)
    (walk-cached-social-graph/followers screen-name #'fb)))


(defun job-auto-follow-back (screen-name)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" screen-name))
  (auto-follow-back screen-name))

(defun start-job-auto-follow-back (screen-name &key (every 3000) (iter 10))
  (submit-job (concatenate 'string "job-auto-follow-back-" screen-name) #'job-auto-follow-back :args (list screen-name) :every every :iter iter :errorhandler t))

;;(collect-all-elements (list "entities.url" "entities.mentions" "text" :_id "user.screen-name") (docs (db.find "tweet" ($ "_id" 137675721693855744) )))
;;*HT*--> home time line

(defun keys (ht)
  (let ((lst))
    (maphash (lambda (k v) (push k lst)) ht)
    (nreverse lst)))

(defun timeline-fields (id)
  (collect-all-elements (list "entities.url" "entities.mentions" "entities.hashtags" "text" :_id "user.screen-name") (docs (db.find "tweet" ($ "_id" id)))))
 
;;137672963506970625 



(defun doc->alist (doc)
  

