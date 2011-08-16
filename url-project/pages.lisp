(in-package :cl-twitter-projects-url-project)


(defun decode (str)
  (let ((str1 (cl-ppcre:regex-replace-all "[\+]"  str " ")))
    (let ((str2 (cl-ppcre:regex-replace-all "%3A"  str1 ":")) ) 
      (let (( str3 (cl-ppcre:regex-replace-all "%2F"  str2 "/")))
	(let ((str4 (cl-ppcre:regex-replace-all "%23"  str3 "#") ))
	  (cl-ppcre:regex-replace-all "%40"  str4 "@") )))))


;;  (hunchentoot:url-decode (strip-utf8 (hunchentoot:url-encode (post-image-title doc)))))
(defun strip-utf8 (str)
  (cl-ppcre:regex-replace-all "%[0-9a-fA-F]{4}" str " "))

(defun strip (str)
  (cl-ppcre:regex-replace-all "[\"▸~]"  str ""))

(defun get-styles-pathname ()
  #p "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project/styles")

(defun style-css-path()
  (concatenate 'string (namestring (get-styles-pathname)) "/style.css"))

(defun cat-style-sheet()
  (with-open-file (stream (style-css-path) :direction :input)
    (slurp-stream stream)))

(defun inject-style-sheet()
    (let ((sheet (cat-style-sheet)))
      ( format nil "<style type=\"text/css\"> ~A </style>" sheet)))

(defun online-style-sheet()
  (format nil "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\"/>"))

(defun post-image-title (doc)
  (format nil "~A" (cl-mongo:get-element "title" doc)))

(defun post-image-filename (doc)
   (format nil "/image/thumbnail_~A.jpeg" (encode-element "base-filename" doc)))
;;----------

(defun cache-link (doc)
  (let ((url  (encode-element "url" doc))
	(fn   (encode-element "base-filename" doc))
	(user (cl-mongo:get-element "user" doc)))
    (format nil "/show-cached?user=~A&url=~A&image=screen_shot_~A.jpeg" user url fn)))


;; lisp has smarter ways to embed the same thing twice !!!
(defun embed-url-ref (doc)
  (let ((encoded-url (encode-element "url" doc))
	(url         (cl-mongo:get-element "url" doc))
	(user        (cl-mongo:get-element "user" doc)))
    (format nil "<a href=\"/redirect?user=~A&url=~A&type=short\" target=\"_blank\"> ~A </a>" user encoded-url url)))

(defun gen-title (doc)
  (let ((title    (post-image-title doc))
	(url      (cl-mongo:get-element "url" doc))
	(url-ref  (embed-url-ref doc)))
    (cl-ppcre:regex-replace-all url  title url-ref)))

;;  (hunchentoot:url-decode (strip-utf8 (hunchentoot:url-encode (post-image-title doc)))))

(defun encode-element (element doc)
  (let ((element (cl-mongo:get-element element doc)))
    (if element 
	(hunchentoot:url-encode element)
	(format nil "[~A]-~A-not-found" (cl-mongo:get-element :_id doc) element))))

;;(defun url-redirect (doc)
;;  (format nil "/redirect?user=~A&url=~A&type=short" (cl-mongo:get-element "user" doc) (cl-mongo:get-element "url" doc)))

(defun resolved-url (doc)
  (cl-mongo:get-element "resolved-url" doc))

(defun redirect-resolved-url (doc)
  (format nil "/redirect?user=~A&url=~A&type=full" (cl-mongo:get-element "user" doc) (encode-element "resolved-url" doc)))

;;TODO this should be fixed

(defvar      MONTHS       '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun month-str->month-number (str)
  (length (member str (reverse MONTHS) :test #'string=)))

(defun shift-time (lst &key (hours 0))
  (let ((shift (* hours 60 60)))
    (destructuring-bind (seconds minutes hours day month year tz) lst
      (multiple-value-bind  (second minute hour day month year dow dst-p tz) 
	  (decode-universal-time (- (encode-universal-time seconds minutes hours day month year) shift))
	(list second minute hour day month year tz)))))

(defun now ()
    (multiple-value-bind  (second minute hour day month year dow dst-p tz)
	(decode-universal-time (get-universal-time))
      (declare (ignore dst-p dow))
      (list year month day hour minute second tz)))

(defun time-list-delta (nlst tlst &optional accum)
  (if (or (null nlst) (null tlst))
      (nreverse accum)
      (time-list-delta (cdr nlst) (cdr tlst) (cons (- (car nlst) (car tlst)) accum ))))

(defun ---time-delta-to-string--- (tstr)
  (destructuring-bind (dy dm dd tdh tdm tds tzd) (time-list-delta (now) (shift-time (parse-twitter-time tstr) :hours 3))
    (declare (ignore tzd))
    (with-output-to-string (stream)
      ;;(format stream "~A  |||" (shift-time (parse-twitter-time tstr) :hours 4))
      ;;(format stream "~A  |||" (now))
      ;;(format stream "~A|~A|~A|" dy dm dd)
      ;;(format stream "{~A|~A|~A}~%" tdh tdm tds)
      ;;(format stream "~A  |||" (parse-twitter-time tstr))
      (when (> dy 0)  (format stream "~D year~:P " dy))
      (when (> dm 0)  (format stream "~D month~:P " dm))
      (when (> dd 0)  (format stream "~D day~:P " dd))
      (when (and (< dy 1) (< dm 1) (or (< dd 1) (> dd 29)))
	(progn
	  (when (> tdh 0) (format stream "~D hour~:P " tdh))
	  ;;(when (< tdh 0) (format stream "~D hour~:P " (+ 24 tdh)))
	  (when (> 1 tdh) (format stream "less than ~D minute~:P " (if (> tdm 0) tdm (+ 60 tdm))))
	    (when (and (= 0 tdh) (= tdm 0) (> tds 0)) (format stream "less than ~D second~:P " tds))
	    (when (and (= 0 tdh) (= tdm 0) (< tds 0)) (format stream "less than ~D second~:P " (+ 60 tds))))
	))))

(defun time-delta (tstr &key (now (get-universal-time)))
  (labels ((to-units (diff multiplier)
	     (/ (- diff (mod diff multiplier)) multiplier)))
    (let ((h (- now (apply #'encode-universal-time (shift-time (parse-twitter-time tstr) :hours 5)))))
      (let* (;;(weeks   (to-units h 604800))
	     (days    (to-units h 86400))
	     (hours   (to-units h 3600))
	     (minutes (to-units h 60))
	     (seconds (to-units h 1)))
	(list
	 days 
	 (mod hours 24)
	 (mod minutes 60)
	 (mod seconds 60))))))

(defun time-delta-to-string (tstr)
  (destructuring-bind (delta-days delta-hours delta-minutes delta-seconds) (time-delta tstr)
    (with-output-to-string (stream)
      ;;(format stream "~S|~S|~S|~S|" delta-days delta-hours delta-minutes delta-seconds)
      (if (< delta-days 0)  
	  (progn
	    (format stream "~D hour~:P "  (+ delta-hours (* 24 delta-days)))
	    (format stream "~D minute~:P " delta-minutes))
	  (progn
	    (if (> delta-days 0)  
		(format stream "~D day~:P " delta-days)
		(progn
		  (when (> delta-hours 0) (format stream "~D hour~:P " delta-hours))
		  (when (= 0 delta-hours) (format stream "~D minute~:P " delta-minutes))
		  (when (and (= 0 delta-hours) (= 0 delta-minutes))  (format stream "~D second~:P " delta-seconds)) )))))))


(defun doc->tweet-id (doc)
  (format nil "id : ~A" (cl-mongo:get-element :_id doc)))

(defun doc->tweet-since (doc)
  (format nil "~A ago" (time-delta-to-string (format nil  "~A" (cl-mongo:get-element "created" doc)))))

(defun doc->tweet-created (doc)
  (format nil  "~A" (cl-mongo:get-element "created" doc)))

(defun thumbs-up-url (doc)
  (format nil "/thumbs-up?user=~A&url=~A" (cl-mongo:get-element "user" doc) (encode-element "url" doc)))

(defun thumbs-up-image ()
  (format nil "/buttons/thumb-up-sm.gif")) 

(defun thumbs-down-url (doc)
  (format nil "/thumbs-down?user=~A&url=~A" (cl-mongo:get-element "user" doc) (encode-element "url" doc)))

(defun thumbs-down-image ()
  (format nil "/buttons/thumb-down-sm.gif")) 

(defun archive-url (doc)
  (format nil "/archive?user=~A&url=~A" (cl-mongo:get-element "user" doc) (encode-element "url" doc)))

(defun archive-image ()
  (format nil "/buttons/archive.jpeg")) 

(defun pin-url (doc)
  (format nil "/pin?user=~A&url=~A" (cl-mongo:get-element "user" doc) (encode-element "url" doc)))

(defun pin-image ()
  (format nil "/buttons/pin.jpeg")) 

(defun create-post (doc)
    (list
     :title                  (gen-title doc)
     :cache                  (cache-link doc)
     :time                   (doc->tweet-created doc)
     :time-delta             (doc->tweet-since doc)
     :resolved-url           (resolved-url  doc)
     :redirect-resolved-url  (redirect-resolved-url doc)
     :id                     (doc->tweet-id doc)
     :pin-url                (pin-url doc)
     :pin                    (pin-image)
     :thumbs-up-url          (thumbs-up-url doc)
     :thumbs-up              (thumbs-up-image)
     :thumbs-down-url        (thumbs-down-url doc)
     :thumbs-down            (thumbs-down-image)
     :archive-url            (archive-url doc)
     :archive                (archive-image)
     :image                  (post-image-filename doc)))
  
;;nreverse will put newer first
(defun collect-posts (&optional (tlf #'create-post))
  (loop for doc in  (collect-docs-with-thumbnails "mohegskunkworks" 0 30)
     collect (funcall tlf doc)))

(defun generate-index-page(&key (style-sheet 'inject-style-sheet))
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template
       (template-path "index2.tmpl")
       (list :style-sheet (funcall style-sheet)
	     :blog-title       "twitter urls.."
	     :blog-posts       (collect-posts))
       :stream stream))))


(defun create-statistic (elem) 
  (let* ((lst ())
	 (tag (car elem))
	 (stats (cdr elem)))
    (dolist  (var stats)
      (push (intern  (string-upcase (car var)) :keyword)  lst)
      (push (cadr var) lst))
    (cons :tag (cons tag (nreverse lst)))))

(defun collect-statistics (&optional (tlf #'create-statistic))
  (loop for doc in  (count-attribute-timeseries "mohegskunkworks")
     collect (funcall tlf doc)))

(defun generate-statistics-page (&key (style-sheet 'inject-style-sheet))
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template
       (template-path "statistics.tmpl")
       (list :style-sheet (funcall style-sheet)
	     :title       "twitter urls statistics"
	     :statistics   (collect-statistics))
       :stream stream))))


#|

|#
;;;
	    
