(in-package :cl-twitter-projects-twitter-bot-project)


(defvar *twitter-mongodb-initialized* nil)

;;(defun make-social-graph-unique-ids 


(defun filter (pred lst)
  (let ((L ()))
    (dolist (el lst)
      (when (funcall pred el) (push el L)))
    (nreverse L)))

(defun logger (log msg)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "logger" )
    (cl-mongo:db.insert log ($ ($ "timestamp" (cl-mongo::make-bson-time)) ($ "message" msg)))))

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
  (cache-social-graph screen-name :maxloop maxloop :cursor cursor :chunk-size chunk-size :type :followers))

(defun cache-social-graph/friends (screen-name &key (maxloop 10000) (cursor -1) (chunk-size 100))
  (cache-social-graph screen-name :maxloop maxloop :cursor cursor :chunk-size chunk-size :type :friends))
 
(defun cache-social-graph  (screen-name &key (maxloop 10000) (cursor -1) (chunk-size 100) (type :followers))
  (let ((result ())
	(loopies 0)
	(graph-type (if (eq type :friends) "FRIENDS/IDS" "FOLLOWERS/IDS"))
	(entities (list "ids" "next-cursor")))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
      (loop 
	 (setf result (car (cl-mongo:collect-all-elements entities (docs (db.find "social-graph-cursor-id" 
										  ($ ($ "screen-name" screen-name) 
										     ($ "command" graph-type) ($ "cursor" cursor)))))))
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


(defun follow-back (screen-name idlst &key (msg (lambda (x) (format t "~S~%" x))))
  (dolist (id idlst)
    (unless (null id)
      (let* ((id-name  (car (get-element "screen-name"  (docs (db.find "twitter-user" ($ "_id" id) :selector ($+ "screen-name" )))))))
	(handler-case
	    (unless (user-a-following-user-b? screen-name id-name)
	      (progn
		(funcall msg (format nil "~A NOT following ~A~%" screen-name id-name))
		(follow id-name)))
	  (twitter-api-condition (c) 
	    (funcall msg (format nil "[id-name : ~S] twitter signaled an error : ~S : ~S ~%" id-name (cl-twitter::return-code c) (cl-twitter::short-message c))))
	  (error (c)
	    (funcall msg (format nil "[id-name : ~S] an error occured : ~S ~%" id-name c))))))))

(defun collect-ids (screen-name &key (type :followers))
  (let ((lst))
    (labels ((collect (idlst)
	       (setf lst (concatenate 'cons idlst lst))))
      (walk-cached-social-graph screen-name #'collect :type type)
      lst)))

(defun collect-ids/followers (screen-name)
  (collect-ids screen-name :type :followers))

(defun collect-ids/friends (screen-name)
  (collect-ids screen-name :type :friends))


(defun walk-cached-social-graph/followers (screen-name func &key (maxloop 10000) (cursor -1) (chunk-size 100))
  (walk-cached-social-graph screen-name func :maxloop maxloop :cursor cursor :chunk-size chunk-size :type :followers))

(defun walk-cached-social-graph/friends (screen-name func &key (maxloop 10000) (cursor -1) (chunk-size 100))
  (walk-cached-social-graph screen-name func :maxloop maxloop :cursor cursor :chunk-size chunk-size :type :friends))

(defun walk-cached-social-graph (screen-name func &key (maxloop 10000) (cursor -1) (chunk-size 100) (type :followers))
  (let ((result ())
	(loopies 1)
	(graph-type (if (eq type :friends) "FRIENDS/IDS" "FOLLOWERS/IDS"))
	(entities (list "ids" "next-cursor")))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
      (loop 
	 (setf result (car (cl-mongo:collect-all-elements entities (docs (db.find "social-graph-cursor-id" 
										  ($ ($ "screen-name" screen-name) 
										     ($ "command" graph-type) ($ "cursor" cursor)))))))
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
(defun initialize-twitter-persistence ()
  (unless *twitter-mongodb-initialized* 
    (progn
      (cl-twitter:use-cache)
      (cl-twitter:use-db :twitter-mongodb-driver)
      (setf *twitter-mongodb-initialized*  'DONE))))
  
(defun dump-social-graph (screen-name)
  (initialize-twitter-persistence)
    (cl-twitter:db-status)
    (db.use "twitter")    
    (cl-twit-repl:get-authenticated-user screen-name)
    (rm "social-graph-cursor-id" ($ "screen-name" screen-name) )
    (cl-twitter:collect-follower-ids screen-name)
    (cl-twitter:collect-friend-ids screen-name)
    (cache-social-graph/followers screen-name)
    (cache-social-graph/friends screen-name))



(defun auto-follow-back (screen-name)
  (labels ((*log (msg)
	     (logger "auto-follow-back" msg))
	   (fb (id-name)
	     (*log (format nil "~S follows ~S" screen-name id-name))
	     (follow-back screen-name id-name :msg #'*log)))
    (*log (format nil "*twitter-mongodb-initialized* ~S " *twitter-mongodb-initialized*))
    (unless *twitter-mongodb-initialized* 
      (progn 
	(*log (format nil "initialize twitter for ~S " screen-name))
	(cl-twitter:use-cache)
	(cl-twitter:use-db :twitter-mongodb-driver)
	(setf *twitter-mongodb-initialized*  'DONE)))
    (*log "use twitter")
    (db.use "twitter")
    (*log (format nil "get twitter credentials for ~S (i.e. log in !)" screen-name)) 
    (cl-twit-repl:get-authenticated-user screen-name)
    (*log "drop the social graph")
    (rm "social-graph-cursor-id" ($ "screen-name" screen-name) )
    (*log (format nil "collect followers for ~S" screen-name))
    (collect-follower-ids screen-name)
    (*log (format nil "collect friends for ~S" screen-name))2
    (collect-friend-ids screen-name)
    (*log "cache the social graph")
    (cache-social-graph/followers screen-name)
    (*log "walk the social graph and follow back...")
    (walk-cached-social-graph/followers screen-name #'fb)
    (*log "done")))


(defun job-auto-follow-back (screen-name)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" screen-name))
  (logger "auto-follow-back" "starting auto-follow-back .......")
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


;;(defun doc->alist (doc)

;;(defun section (l r inter excl-l excl-r)
;;  (cond (eq (car l) (car r)) (section (cdr l) (cdr r) (cons (car l) inter) excl-l excl-r)
	
(defun val* (el)
  (car el))

(defun label* (el)
  (cadr el))

(defun uniq* (l &optional accum)
  (cond ((null l) accum)
	((member (car l) accum) (uniq* (cdr l) accum))
	( t (uniq* (cdr l) (cons (car l) accum)))))

(defun section (l r) 
  (labels ((sorter (x y)
	     (if (= (car x) (car y))
		 (char> (cadr x) (cadr y))
		 (> (car x) (car y)))))
  (let* ((l1 (mapcar (lambda (i) (list i #\L)) (uniq* l)))
	 (r1 (mapcar (lambda (i) (list i #\R)) (uniq* r)))
	 (c  (concatenate 'cons l1 r1))
	 (s  (sort c #'sorter)))
    s)))

(defun exclusive* (s accum &key label)
  (let ((elem (car s))
	(look-ahead (cadr s)))
    (cond ((null s) (nreverse accum))
	  ((and (char= label (label* elem)) (null look-ahead)) (exclusive* (cdr s) (cons (val* elem) accum) :label label))
	  ((null look-ahead) (exclusive* (cdr s) accum :label label))
	  ((and (char= label (label* elem)) (char= label (label* look-ahead))) (exclusive* (cdr s) (cons (val* elem) accum) :label label))
	  ((= (val* elem) (val* look-ahead)) (exclusive* (cdr (cdr s)) accum :label label))
	  ((char= label (label* elem))  (exclusive* (cdr s) (cons (val* elem) accum) :label label))
	  (t (exclusive* (cdr s) accum :label label)))))


(defun l-not-in-r (l r)
  (exclusive* (section l r) () :label #\L))

(defun followers-not-friends (screen-name)
  (let ((followers (collect-ids/followers screen-name))
	(friends   (collect-ids/friends screen-name)))
    (l-not-in-r followers friends)))

(defun friends-not-followers (screen-name)
  (let ((followers (collect-ids/followers screen-name))
	(friends   (collect-ids/friends screen-name)))
    (l-not-in-r friends followers)))

(defun lst-value* (key lst)
  (cadr (find key lst :key #'first)))

(defun frfo (fr fo)
  (let ((d (- fr fo))
	(v (+ fr fo)))
    (cond ((zerop v) 1)
	  ( t (* 1.0  (/ d v))))))
	
(defun quality-measure (elem)
  (let* ((fo (lst-value* :followers-count elem))
	(fr (lst-value* :friends-count elem))
	(mes (frfo fr fo))
	(id   (lst-value* :_id elem))
	(name (lst-value* :screen-name elem)))
    (zip '(:_id :screen-name :measure :followers-count :friends-count) (list id name mes fo fr))))

(defun info-sources (el)
  (< (lst-value* :measure el) -0.5))


(defun bad-friends (el)
  (and  (not (info-sources el))  (not (find (lst-value* :screen-name el) (list "loriabys") :test #'equalp))))
  
;;  


(defun map-graph-data (f idlst)
  (let ((lst))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
      (dolist (id idlst)
	(let* ((docs (docs (db.find "twitter-user" ($ "_id" id) :selector ($+ "screen-name" "followers-count" "friends-count" "statuses-count"))))
	       (fields (list :_id "screen-name" "followers-count" "friends-count" "statuses-count"))
	       (kw     (mapcar (lambda (w) (intern (string-upcase w) :keyword)) (cdr fields)))
	       (vallst (collect-all-elements fields docs))
	     (elems  (mapcar (lambda (lst) (zip (cons :_id kw) lst)) vallst))) 
	  (dolist (el elems)
	    (push (funcall f el) lst)))))
    (nreverse lst)))

(defun follow-no-more* (el)
  (let ((screen-name (lst-value* :screen-name el)))
    (handler-case
	(cl-twitter:unfollow screen-name)
      (twitter-api-condition (c) 
	(format nil "[id-name : ~S] twitter signaled an error : ~S : ~S ~%" screen-name (cl-twitter::return-code c) (cl-twitter::short-message c)))
      (error (c)
	(format nil "[id-name : ~S] an error occured : ~S ~%" screen-name c)))))



;;
;;  UNFOLLOW BAD FRIENDS 
;; dump the social graph
;; find out who is not following; filter out the bad friends (qm > 0 I believe..)
;; then unfolow...

(defun unfollow-bad-friends (screen-name &key (dump-graph))
  (when dump-graph (dump-social-graph screen-name))
  (mapcar #'follow-no-more* (filter #'bad-friends (map-graph-data  #'quality-measure  (friends-not-followers screen-name )))))

  

;;
;; FIND LIKELY FOLLOWERS....
;; This is based on the occupywallstr social graph. This needs to be dumped and cached ....
;;
;; Then the steps are :
;;
;;(defvar *TT* (map-graph-data #'quality-measure (collect-ids/followers "occupywallst")))
;;(defvar *INFO* (filter #'info-sources *TT*))
;;(mapcar #'save-follow  (filter (lambda (el) (< (lst-value* :measure el) -0.98)) *INFO*))


(defun save-follow (el)
  (let ((id-name (lst-value* :screen-name el)))
    (save-follow* id-name)))

(defun save-follow* (id-name)
    (handler-case
	(cl-twitter:follow id-name)
      (twitter-api-condition (c) 
	(format nil "[id-name : ~S] twitter signaled an error : ~S : ~S ~%" id-name (cl-twitter::return-code c) (cl-twitter::short-message c)))
      (error (c)
	(format nil "[id-name : ~S] an error occured : ~S ~%" id-name c))))


(defun targets (el)
  (and (> (lst-value* :measure el) 0) (< (lst-value* :measure el) 0.5)))

;;(defvar *TARGETS* (filter #'targets *TT*))

(defun el->document (el)
  (let ((ht (make-hash-table :test #'equalp)))
    (setf (gethash "screen-name"  ht) (lst-value* :screen-name el))
    (setf (gethash "_id"  ht) (lst-value* :_id el))
    (setf (gethash "friends"  ht) (lst-value* :friends-count el))
    (setf (gethash "followers"  ht) (lst-value* :followers-count el))
    (setf (gethash "measure"  ht) (lst-value* :measure el))
    (ht->document ht)))

(defun save-targets (screen-name lst)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db screen-name)
    (dolist (el lst)
      (db.insert "targets" (el->document el)))))

;;
;;
;; follow targets : follow 100 targets...
;;
;;
(defun follow-targets (screen-name &key (limit 100))
  (cl-twit-repl:get-authenticated-user screen-name)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db screen-name)
    (let ((lst (collect-all-elements (list :_id "screen-name") (docs (db.find "targets" (cl-mongo:$exists "followed" nil) :limit limit)))))
      (dolist (el lst)
	(format t "~S ~S ~%" (car el) (cadr el))
	(db.update "targets" ($ "_id" (car el)) ($set "followed" "Y") :upsert t :multi t)
	(format t "following ~S~%" (cadr el))
	(save-follow* (cadr el))))))

    
      
;; Inspect followbacks....

;; 1 dump social graph
;; 2 for each follower if also in 'target' set flag FOLLOWED_BACK to Y.. 

(defun lst->ht (ids)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (id ids)
      (setf (gethash id ht) 1))
    ht))

(defvar *FOLLOW-BACK-MESSAGE* "The chairman thanks you for the follow back http://bit.ly/uhQuXG")

(defun send-message/follow-ack (id-name)
  (handler-case
      (cl-twitter:send-message id-name *FOLLOW-BACK-MESSAGE*)
    (twitter-api-condition (c) 
      (format nil "[id-name : ~S] twitter signaled an error : ~S : ~S ~%" id-name (cl-twitter::return-code c) (cl-twitter::short-message c)))
    (error (c)
      (format nil "[id-name : ~S] an error occured : ~S ~%" id-name c))))

(defun flag-follow-backs (screen-name &key (limit 1000000) (dump-graph t))
  (when dump-graph (dump-social-graph screen-name))
  (let ((followers (lst->ht (collect-ids/followers screen-name))))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db screen-name)
      (let ((lst (collect-all-elements (list :_id "screen-name") (docs (db.find "targets" ($ ($exists "followed_back" nil) (cl-mongo:$exists "followed" t)) :limit limit)))))
	(dolist (el lst)
	  (when (gethash (car el) followers)
	    (format t "~S ~S ~%" (car el) (cadr el))
	    (send-message/follow-ack (cadr el))
	    (db.update "targets" ($ "_id" (car el)) ($set "followed_back" "Y") :upsert t :multi t)))))))

  
(defun user->mentioned-target (user)
  (let ((ht (make-hash-table :test #'equalp)))
    (setf (gethash "screen-name"  ht) (twitter-user-screen-name user))
    (setf (gethash "_id"  ht) (twitter-user-id user))
    (setf (gethash "friends"  ht) (twitter-user-friends-count user))
    (setf (gethash "followers"  ht) (twitter-user-followers-count user))
    (setf (gethash "measure"  ht) (frfo (twitter-user-friends-count user) (twitter-user-followers-count user)))
    (setf (gethash "followed" ht) "Y")
    (setf (gethash "mentioned" ht) "Y")
    (ht->document ht)))

(defun uniq-users (lst)
  (let ((ht (make-hash-table :test #'equalp))
	(set))
    (dolist (user lst)
      (setf (gethash (twitter-user-id user) ht) user))
    (maphash (lambda (k v) (push v set)) ht)
    set))

(defun follow-back-mentions (screen-name)
  (initialize-twitter-persistence)
  (cl-twit-repl:get-authenticated-user screen-name)
  (let ((users (uniq-users (mapcar #'tweet-user (mentions :count 200)))))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db screen-name)
      (dolist (user users)
	(when (zerop (ret (db.count "targets" ($ "_id" (twitter-user-id user)))))
	  (progn
	    (format t "~S ~S ~%" (twitter-user-id  user) (twitter-user-screen-name user))
	    (db.insert "targets" (user->mentioned-target user))
	    (format t "following ~S~%" (twitter-user-screen-name user))
	    (save-follow* (twitter-user-screen-name user))))))))

#|

I'd say the "best practices" for developing your own quote tweet function would be:

Original author's full screen name should appear within the tweet
Quoted tweet text should be wrapped in quotes
Any truncation should be made obvious through ellipses or other common editorial symbols
All still fitting within 140 characters

https://dev.twitter.com/discussions/1748

|#

;;;;------------------------------------  
(defun init-ht (ht id)
  (if (< id 0) ht
      (progn 
	(setf (gethash id ht) 0) 
	(init-ht ht (- id 1)))))
	
(defun do-random (repeats range)
  (let ((ht (init-ht (make-hash-table) range)))
    (loop
       (let ((nxt (random range)))
	 (setf (gethash nxt ht) (+ 1 (gethash nxt ht)))
	 (decf repeats)
	 (when (zerop repeats) (return ht))))))

(defun disp (ht)
  (maphash (lambda (k v) (format t "~A : ~A ~%" k v)) ht))

(defun ht->lst (ht)
  (let ((lst))
    (maphash (lambda (k v) (push v lst)) ht)
    lst))

      ;;

;;retweet bot..
;;
;; dailykos, thinkprogress, thenation, daily work (rss feed),ourfuturedotorg


(defun select-retweet-from-sources (screen-name &key (max 1) (retweets 1))
  (let* ((lst)
	 (tweets (collect-user-list-statuses screen-name  "sources" :max max))
	 (retweets (if (< retweets (hash-table-count tweets)) retweets (hash-table-count tweets))))
    (unless (zerop retweets)
      (loop
	 (let* ((index (random (hash-table-count tweets)))
		(tweet (nth index (ht->lst tweets))))
	   (push tweet lst)
	   (remhash (tweet-id tweet) tweets))
	 (decf retweets)
	 (when (or (zerop retweets) (zerop (hash-table-count tweets))) (return lst))))))
  
(defun retweet->document (tweet)
  (let ((ht (make-hash-table :test #'equalp)))
    (setf (gethash "_id"          ht) (tweet-id tweet))
    (setf (gethash "screen-name"  ht) (twitter-user-screen-name (tweet-user tweet)))
    (setf (gethash "txt"          ht) (tweet-text tweet))
    (setf (gethash "timestamp"    ht) (cl-mongo::make-bson-time))
    (ht->document ht)))

(defun select-retweets (screen-name &key (max 1) (retweets 1))
  (let ((retweets (select-retweet-from-sources screen-name :max max :retweets retweets)))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db screen-name)
      (dolist (tweet retweets)
	(when (zerop (ret (db.count "retweets" ($ "_id" (tweet-id tweet)))))
	  (db.insert "retweets" (retweet->document tweet)))))))

(defun job-select-retweets (screen-name)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" screen-name))
  (select-retweets screen-name :max 4 :retweets 25))

(defun start-job-select-retweets (screen-name &key (every 1384) (iter 10))
  (submit-job (concatenate 'string "job-select-retweets-" screen-name) #'job-select-retweets :args (list screen-name) :every every :iter iter :errorhandler t))

;;
;; Retweet group time line
;;
(defun user-list-timeline-retweets (screen-name &key (max 1))
  (let ((retweets (ht->lst (user-list-timeline screen-name :max-per-list max))))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db screen-name)
      (progn 
	(rm "retweets" (cl-mongo:$exists "retweeted" nil))
	(dolist (tweet retweets)
	  (when (zerop (ret (db.count "retweets" ($ "_id" (tweet-id tweet)))))
	    (db.insert "retweets" (retweet->document tweet))))))))
  
(defun job-user-list-timeline-retweets (screen-name)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" screen-name))
  (user-list-timeline-retweets screen-name :max 1))

(defun start-job-user-list-timeline-retweets (screen-name &key (every 1384) (iter 10) (maxerror 20))
  (submit-job (concatenate 'string "job-user-list-timeline-retweets" screen-name) #'job-user-list-timeline-retweets 
	      :args (list screen-name) :every every :iter iter :errorhandler t :maxerror maxerror))


(defun fmt-retweet (user txt tag)
  (let ((fmt-txt (if tag 
		     (format nil "RT @~A: \"~A\" ~A" user txt tag)
		     (format nil "RT @~A: \"~A\" " user txt))))
    (when (< (length fmt-txt) 140)
      fmt-txt)))

(defun soft-retweet (id txt)
  (if id
      (tweet txt :in-reply-to-status-id id)
      (tweet txt)))

(defun retweet-bot (screen-name)
  (cl-twit-repl:get-authenticated-user screen-name)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db screen-name)
    (let ((lst (collect-all-elements (list :_id "screen-name" "txt") (docs (db.find "retweets" (cl-mongo:$exists "retweeted" nil) :limit 1)))))
      (dolist (el lst)
	(format t "~S~%" el)
	(if (> 3 (random 4))
	    (retweet-status  (car el))
	    (progn
	      (let ((txt (fmt-retweet (cadr el) (caddr el) "#ows"))) 
		(if txt
		    (soft-retweet (car el) txt)
		    (retweet-status  (car el))))))
	(db.update "retweets" ($ "_id" (car el)) ($set "retweeted" (cl-mongo::make-bson-time)) :upsert t :multi t)))))


(defun job-retweet-bot (screen-name)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" screen-name))
  (retweet-bot screen-name))

(defun start-job-retweet-bot (screen-name &key (every 183) (iter 10) (maxerror 5000))
  (submit-job (concatenate 'string "job-retweet-bot-" screen-name) #'job-retweet-bot :args (list screen-name) :every every :iter iter :errorhandler t :maxerror maxerror))

;;
;; unfollow-bad-friends
;; follow-targets

(defun next-follow-batch (screen-name)
  (dump-social-graph screen-name)
  (flag-follow-backs screen-name :dump-graph nil)
  (unfollow-bad-friends screen-name)

  (follow-targets screen-name))


;;(cl-twit-repl::show (rate-limit-status))
;;(db.use "darealmaozedong")
;;(pp (db.count "retweets" (cl-mongo:$exists "retweeted" nil) ))
;;(start-job-user-list-timeline-retweets "darealmaozedong" :every (* 42 61) :iter 24)
;;(start-job-retweet-bot "darealmaozedong" :every (* 61 2) :iter 300)
;;(start-job-twitter-bot "darealmaozedong" :every (* 56 16 ) :iter 320)