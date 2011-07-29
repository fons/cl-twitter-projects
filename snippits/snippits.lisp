
(mapdoc (lambda (k v) (format t "~S:~S~%" k v)) (car (docs (db.find "tweet"  ($exists "entities.urls.url" t)  :limit 1000  :selector ($+ "entities.urls.url" "text" "user.screen-name")))))
"text":"Mysterious number 6174 | plus.maths.org: http://t.co/3kQVqm6 via @addthis"
"entities":<DOCUMENT> : { 
   elements : 1}

'(user screen-name)

 
    

(cond ((type-of 'document (get-element (car elems) doc))  (find-field (get-element (car elems) doc) (cdr elem) accum)
       (nil (cadr elem)     accum)
       (t                                                 (get-element (car elems) doc)
 
---
(get-element "ids" (docs (cl-mongo:db.find "social-graph-cursor-id" :all :selector :ids)))


(pp (db.count "tweet" ($ "user.screen-name" "bradfordcross")))
;;-----------------------------------------------------------
;;should use twitter.....
;;				     
(defun id->screen-name (id)
  (cl-mongo:get-element "screen-name" (car (cl-mongo:docs (cl-mongo:db.find "twitter-user" ($ "id" id) :selector "screen-name")))))

(defun social-graph-friends (screen-name)
  (let* ((docs  (docs (cl-mongo:db.find "social-graph-cursor-id" ($ "screen-name" screen-name) :selector ($+ "screen-name" "ids")))) 
	 (ids   (cadr (car (cl-mongo::collect-all-elements (list "screen-name" "ids") docs)))))
	 (mapcar (lambda (id) (id->screen-name id)) ids)))


(defun tweets-with-url (screen-name &optional (last-id 0))
  (let ((query-doc (cl-mongo:$ (cl-mongo:$ "user.screen-name" screen-name) (cl-mongo:$exists "entities.urls.url" t) (cl-mongo:$> "_id" last-id)))
	(selector  ($+ "entities.urls.url" "text" "user.screen-name" "_id")))
    (cl-mongo::collect-all-elements (list :_id "user.screen-name" "text" "entities.urls.url" ) (docs (iter (db.find "tweet" query-doc :limit 1000  :selector selector))))))
  
;;----should possibly in a different data-base.... 
(defun save-state (name id)
  (db.update "url-project" ($ "name" name)  ($ ($set "last" id) ($push  "ids" id)) :upsert t))

(defun get-state (name)
  
  (cl-mongo:get-element "last" (cl-mongo:docs (db.find "url-project" ($ "name" name) :selector ($+ "last")))))

(defun sort-tweet-ids (lst)
  (let ((tweet-ids (mapcar (lambda (el) (car el)) lst)))
    (labels ((get-max-id (max-id id)
	       (if (> id max-id)
		   id
		   max-id)))
      (reduce #'get-max-id tweet-ids :initial-value -1))))


(defun get-last-id (name)
  (car (get-state name)))

(defun get-tweet-list (screen-name &optional (last-id -1))
  (mapcar (lambda (screen-name) (tweets-with-url screen-name last-id)) (social-graph-friends screen-name)))

(defun get-max-id (tweet-list)
  (car (sort (mapcar #'sort-tweet-ids tweet-list)  #'>)))

(defun save-state (screen-name max-id)
  (db.update "url-project" ($ "name" screen-name)  ($ ($set "last" max-id) ($push  "ids" max-id)) :upsert t))


(defun get-friends-urls (screen-name)
  (let* ((last-id     (or (get-last-id screen-name) 0))
	 (tweet-list  (get-tweet-list screen-name last-id))
	 (max-id      (get-max-id tweet-list)))
    (progn 
      (when (> max-id last-id) (save-state  screen-name max-id))
      tweet-list)))

;;(car (sort (mapcar #'sort-tweet-ids (mapcar #'tweets-with-url (social-graph-friends "mohegskunkworks"))) #'>))

;(mapcar #'tweets-with-url (social-graph-friends "mohegskunkworks"))

;;(mapdoc (lambda (k v) (format t "~S:~S~%" k v)) (car (docs (db.find "tweet"  ($exists "entities.urls.url" t)  :limit 1000  :selector ($+ "entities.urls.url" "text" "user.screen-name")))))


;;:limit 1000  :selector ($+ "entities.urls.url" "text" "user.screen-name")))
