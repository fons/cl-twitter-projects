(in-package :cl-twitter-projects-url-project)

(defun id->screen-name (id)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
    (cl-mongo:get-element "screen-name" (car (cl-mongo:docs (cl-mongo:db.find "twitter-user" ($ "id" id) :selector "screen-name"))))))

(defun social-graph-friends (screen-name)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
    (let* ((docs  (docs (cl-mongo:db.find "social-graph-cursor-id" ($ "screen-name" screen-name) :selector ($+ "screen-name" "ids")))) 
	   (ids   (cadr (car (cl-mongo::collect-all-elements (list "screen-name" "ids") docs)))))
      (mapcar (lambda (id) (id->screen-name id)) ids))))


(defun tweets-with-url (screen-name &optional (last-id 0))
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
    (let ((query-doc (cl-mongo:$ (cl-mongo:$ "user.screen-name" screen-name) (cl-mongo:$exists "entities.urls.url" t) (cl-mongo:$> "_id" last-id)))
	  (selector  ($+ "entities.urls.url" "text" "user.screen-name" "_id" "retweet-count" "created-at")))
      (cl-mongo::collect-all-elements (list :_id "user.screen-name" "text" "entities.urls.url" "retweet-count" "created-at") 
				      (docs (iter (db.find "tweet" query-doc :limit 1000  :selector selector)))))))

(defun get-tweet (id)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "twitter" )
    (docs (iter (db.find "tweet" ($ "_id" id) :limit 1)))))
  
;;---------------------------------------------------------------------------------------------------------------
(defun get-state (name)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
    (nreverse (cl-mongo:get-element "ids" (car (cl-mongo:docs (db.find "state" ($ "name" name) :selector ($+ "ids"))))))))

(defun save-state (screen-name max-id)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
    (db.update "state" ($ "name" screen-name)  ($push  "ids" max-id) :upsert t)))

(defun get-last-id (name)
  (car (get-state name)))

(defun get-prev-id (name)
  (cadr (get-state name)))

;;---------------------------------------------------

(defun get-tweet-list (screen-name &optional (last-id -1))
  (mapcan (lambda (screen-name) (tweets-with-url screen-name last-id)) (social-graph-friends screen-name)))

(defun get-max-id (tweet-list)
      (car (sort (mapcar #'car tweet-list)  #'>)))

(defun get-friends-urls (screen-name)
  (let* ((last-id     (or (get-last-id screen-name) 0))
	 (tweet-list  (get-tweet-list screen-name last-id))
	 (max-id      (or (get-max-id tweet-list) -1)))
    (progn 
      (when (> max-id last-id) (save-state  screen-name max-id))
      tweet-list)))

(defun url->filename (url)
  (cl-ppcre:regex-replace-all "[/.:]" url "_"))

;;(defun set-base-filename (screen-name doc)
;;  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
;;    (db.update screen-name ($ "_id" (cl-mongo:get-element :_id doc))  ($ ($set "base-filename" (url->filename (cl-mongo:get-element "url" doc)) )) :upsert t)))

;; see tweets-with-urls for the structure of the list
;;TODO this does not work for multiple urls !!!
;; all entries are saved with the same id..

(defun process-url-item (screen-name item)
  (destructuring-bind (id author tweet urls retweeted created) item
    (dolist (url urls)
      (db.insert screen-name ($ ($ "_id"           id) 
				($ "user"          screen-name)
				($ "title"         (concatenate 'string author " : " tweet)) 
				($ "url"           url)
				($ "retweeted"     retweeted)
				($ "created"       created)
				($ "base-filename" (url->filename url))
				($ "processed"     0))))))


(defun process-all-urls (screen-name list)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
    (length (mapcar (lambda (item) (process-url-item screen-name item)) list))))

;;    (get-element "n" (car (docs (db.count screen-name :all))))))

;;------------------------------------------------------------------------------------------------------------  
;; this finds new urls and inserts them in a special collection with the same name as the screen name.
(defun process-latest-friend-urls (screen-name)
  (process-all-urls screen-name (get-friends-urls screen-name)))

;;------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------

(defun change-field-count (user selector field &key (change 1))
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
    (db.update user ($ (car selector) (cadr selector))  ($inc field change)  :upsert t :multi t)))


;;------------------------------------------------------------------------------------------------------------


;;(defun docs-missing-base-filename (screen-name limit)
;;  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
;;    (docs  (db.find screen-name ($exists "base-filename" nil) :limit limit))))
  
;;(defun set-base-filenames (screen-name limit)
;;  (mapcar (lambda (doc) (set-base-filename screen-name doc)) (docs-missing-base-filename screen-name limit)))

;;(defun unset-base-filename (screen-name doc)
;;  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
;;    (db.update screen-name ($ "_id" (cl-mongo:get-element :_id doc))  ($ ($unset "base-filename" (url->filename (cl-mongo:get-element "url" doc)) )) :upsert t)))

;;(defun unset-base-filenames (screen-name limit)
;;  (mapcar (lambda (doc) (unset-base-filename screen-name doc)) (docs-to-process screen-name limit)))


(defun with-exception-handled (f &rest args)
  (handler-case 
      (apply f args)
    (error(c)
      (declare (ignore c)))))


;;------------------------------------------------------------------------------------------------------------
(defun thumbnail-image-filename (doc)
   (format nil "/Users/fons/Data/twitter-project/thumbnail_~A.jpeg" (cl-mongo:get-element "base-filename" doc)))

(defun thumbnail-exists (doc &key (test #'probe-file))
  (if (with-exception-handled test (thumbnail-image-filename doc))
      doc
      nil))

(defun sort-doc-list (doc-list)
  (labels ((sort-func (d1 d2)
	     (let ((id1 (cl-mongo:get-element :_id d1))
		   (id2 (cl-mongo:get-element :_id d2)))
	       (> id1 id2))))
    (sort doc-list #'sort-func)))

(defun docs-having-base-filenames (screen-name &optional (start-id 0))
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
    (docs (iter (db.find screen-name ($ ($> "_id" start-id) ($ ($exists "archived" nil) ($exists "base-filename" t))) :limit 100)))))

;;---------------------------------------------------------------------------------------------------  

(defun collect-docs-with-thumbnails (screen-name &optional (start-id 0)  (max 20))
  (let ((lst)
	(count 0))
    (dolist (elem (mapcar #'thumbnail-exists (sort-doc-list (docs-having-base-filenames screen-name start-id))))
      (when (and elem (or (< count max)  (< max 0))) (push elem lst))
      (when elem (incf count)))
    (nreverse lst)))

(defun collect-docs-without-thumbnails (screen-name &optional (start-id 0)  (max 20))
  (let ((lst)
	(count 0))
    (dolist (elem (mapcar (lambda (doc) (thumbnail-exists doc :test (lambda (x) (not (probe-file x)))))  (docs-having-base-filenames screen-name start-id)))
      (when (and elem (or (< count max)  (< max 0))) (push elem lst))
      (when elem (incf count)))
    (nreverse lst)))

;;-----------------------------------------------------------------------------------------
;;(defun docs-to-process (screen-name limit)
;;  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
;;    (docs (db.find screen-name ($ "processed" 0)  :selector "url" :limit limit))))

(defun take-screen-shot (url)
  (sb-ext:run-program "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project/screen-shot.ksh" (list url (url->filename url))))

(defun urls-to-process (screen-name start-id limit)
  (cl-mongo:get-element "url" (collect-docs-without-thumbnails screen-name start-id limit)))

(defun get-screen-shots (screen-name &key (limit 20))
  (mapcar #'take-screen-shot (urls-to-process screen-name 0 limit)))

;;----------------------------------------------------------------------------------------

(defun ping-url (url)
  (multiple-value-bind (response status-code headers puri-url must-close phrase) (drakma:http-request url :redirect 5
												      :auto-referer nil 
												      :want-stream t )
    (declare (ignore response headers phrase))
    (close must-close)
    (values (puri:render-uri puri-url nil) status-code)))

;;don't use the iterator as it does not respect the limit
(defun docs-without-resolved-url (screen-name &key (limit 20))
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
    (docs (db.find screen-name ($exists "resolved-url" nil) :limit limit))))

;;(with-exception-handled #' ping-url (nth 18 (get-element "url" (docs-without-resolved-url "mohegskunkworks"))))

(defun resolve-shortened-urls (screen-name doc)
  (let ((resolved-url (with-exception-handled #'ping-url (get-element "url" doc))))
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )
      (db.update screen-name ($ "_id" (get-element :_id doc))  ($set "resolved-url"  resolved-url)  :upsert t))))
    
(defun resolve-urls (screen-name &key (limit 20))
  (mapcar (lambda (doc) (resolve-shortened-urls screen-name doc)) (docs-without-resolved-url screen-name :limit limit)))

;;(defun gen-statistics (screen-name)
  

;;(defun collect-thumbnails (screen-name limit &optional (start-id 0))
;;  (loop for filename in (mapcan (lambda (fn) (with-exception-handled #'probe-file fn)) (mapcar #'thumbnail-image-filename (docs-having-base-filenames screen-name limit start-id)))
;;     if filename
;;     collect filename))

;;(defun docs-with-file-basenames (screen-name limit &optional (start-id 0)) 
;;  (docs (db.find screen-name ($ ($> "_id" start-id) ($exists "base-filename" t)) :limit limit)))

(defun parse-twitter-time (str)
  (destructuring-bind (day month day-of-month time offset year) (split-sequence:SPLIT-SEQUENCE #\space str)
    (declare (ignore day))
    (destructuring-bind (hour minutes seconds) (split-sequence:SPLIT-SEQUENCE #\: time)
      (list   (parse-integer seconds) 
	      (parse-integer minutes) 
	      (parse-integer hour)
	      (parse-integer day-of-month) 
	      (month-str->month-number month) 
	      (parse-integer year) 
	      (parse-integer offset)))))
	      


;;encode-universal-time second minute hour date month year &optional time-zone

;;decode-universal-time universal-time &optional time-zone

;;The time specified by universal-time in Universal Time format is converted to Decoded Time format. 
;;Nine values are returned: second, minute, hour, date, month, year, day-of-week, daylight-saving-time-p, and time-zone.

;;(cl-mongo::make-bson-time (cl-mongo::gmt-to-bson-time (encode-universal-time 30 11 19 11 6 2011)))

;;(cl-mongo::make-bson-time (cl-mongo::gmt-to-bson-time (apply #'encode-universal-time (cdr (nreverse (parse-twitter-time (car (get-element "created" (docs (db.find "mohegskunkworks" ($exists "created" t) :limit 2 ))))))))))

;;(decode-universal-time (cl-mongo::bson-time-to-ut (cl-mongo::make-bson-time (cl-mongo::gmt-to-bson-time (apply #'encode-universal-time (cdr (nreverse (parse-twitter-time (car (get-element "created" (docs (db.find "mohegskunkworks" ($exists "created" t) :limit 2 ))))))))))))

(defun add-timestamp (doc)
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )  
    (let* ((time-elements (parse-twitter-time (get-element "created" doc)))
	   (screen-name   (get-element "user" doc))
	   (_id           (get-element :_id doc))
	   (timestamp (cl-mongo::make-bson-time (cl-mongo::gmt-to-bson-time (apply #'encode-universal-time time-elements)))))
      (db.update screen-name ($ "_id" _id)  ($set "timestamp" timestamp)  :upsert t))))

(defun add-missing-timestamp (screen-name &key (limit 50))
  (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project")  
    (let ((doc-lst (docs (db.find screen-name ($ ($exists "timestamp" nil)  ($exists "created" t) ) :limit limit))))
      (length (mapcar #'add-timestamp doc-lst)))))

;;------------------------------------


(defun midnight (&key (hours 0) (days 0))
 (multiple-value-bind  (second minute hour day month year dow dst-p tz)
  (decode-universal-time (- (get-universal-time) (+ (* 86400 days ) (* hours 3600))))
   (encode-universal-time 0 0 0 day month year)))

(defun start-of-day (&key (hours 0) (days 0))
  (cl-mongo::make-bson-time (cl-mongo::gmt-to-bson-time (midnight :hours hours  :days days))))

(defun query-attribute> (screen-name attribute days-since &key (exists t) )
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )  
      (let ((ts (start-of-day :days days-since)))
	(get-element "n" (docs (db.count screen-name ($ ($> "timestamp" ts) ($exists attribute exists))))))))

(defun query-attribute< (screen-name attribute days-since &key (exists t) )
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )  
      (let ((ts (start-of-day :days days-since)))
	(get-element "n" (docs (db.count screen-name ($ ($< "timestamp" ts)  ($exists attribute exists))))))))

(defun query-attribute (screen-name attribute days-since &key (exists t) (since t))
  (if since
      (query-attribute> screen-name attribute days-since :exists exists )      
      (query-attribute< screen-name attribute days-since :exists exists )))

(defun query-total< (screen-name days-since)
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )  
      (let ((ts (start-of-day :days days-since)))
	(get-element "n" (docs (db.count screen-name ($<= "timestamp" ts )))))))

(defun query-total> (screen-name days-since)
    (with-mongo-connection (:host cl-mongo:*mongo-default-host* :port cl-mongo:*mongo-default-port* :db "url-project" )  
      (let ((ts (start-of-day :days days-since)))
	(get-element "n" (docs (db.count screen-name ($>= "timestamp" ts)))))))

(defun query-total (screen-name days-since &key (since t))
  (if since
      (query-total> screen-name days-since)      
      (query-total< screen-name days-since)))

(defun count-attributes (screen-name days-since &key (since t) (attributes (list "archived" "pinned" "liked" "disliked")))
  (labels ((count-attr (kw)
	     (query-attribute screen-name kw days-since :exists t :since since)))
    (acons "total" (query-total screen-name days-since :since since) (nreverse (pairlis attributes (mapcar #'count-attr attributes))))))
    
(defun attribute-difference (lhs rhs &optional accum)
  (if (null lhs)
      (nreverse accum)
      (let ((delta ( - (cadar lhs) (cadar rhs))))
	(attribute-difference  (cdr lhs) (cdr rhs) (acons (caar lhs) delta accum)))))

;;(format nil "~D day~:P" 2)

(defun count-attribute-timeseries (screen-name &key (interval (list 0 1 2 7 14)))
  (nreverse (acons "total" (count-attributes screen-name 0 :since nil)
		   (pairlis (mapcar (lambda (d) (format nil "~D day~:P" d)) interval) (mapcar (lambda (days-since) (count-attributes screen-name days-since)) interval)))))


;;(a1 a2 a2 a3) ==> a1, f(a2,a1), f(a3,a2) , a3

;;(a1,a2,a3,a4,a5,a6 ==> (a2,a1) , (a3,a2) , (a4, a3) ==> zip ( (a2,a3,a4) (a1,a2,a3) )==> zip (cdr L, L)

;;(mapcar (lambda (s t1) (list s t1) ) (cdr *T*) *T*)
;;(mapcar (lambda (s t1) (list s t1) ) (cdr *T*) *T*)
(defun zipper (l)
  (mapcar (lambda (s t1) (list s t1) ) (cdr l) l))