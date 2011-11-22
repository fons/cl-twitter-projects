(in-package :cl-twitter-projects-url-project)

;;;---------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------
(defvar *url-project-server*  nil)
(defvar *server-session-id* nil)
;;(setf hunchentoot:*message-log-pathname* "/tmp/error.file")
(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(defvar *utf-8* (flex:make-external-format :utf-8))

(defvar *template-pathname* (pathname (concatenate 'string (namestring (user-homedir-pathname)) "Repo/git.hub/cl-twitter-projects/url-project/templates/")))
(defvar *image-file-directory-root* (format nil "~ARepo/git.hub/cl-twitter-projects/url-project" (namestring (user-homedir-pathname))))
(defvar *redirect-html* (format nil "~ARepo/git.hub/cl-twitter-projects/url-project/templates/redirect.html" (namestring (user-homedir-pathname))))

(setf hunchentoot:*hunchentoot-default-external-format* *utf-8*)


(defun index-unless(d)
  (if (> (length d) 1)
      d
      "index.html"))

;;use this for cache busting the urls...
(defun new-session-id ()
  (setf *server-session-id* (mod (get-universal-time) 65537)))

(defun session-id ()
  (random *server-session-id*))

(defun slurp-stream(stream)
  ;;from
  ;;www.emmett.ca/~sabetts/slurp.html
  ;;
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun get-template-pathname ()
  *template-pathname*)


(defun template-path (tmpl)
  (make-pathname :directory (namestring (get-template-pathname) )  :name tmpl))

(defun generate-error-page(trace msg)
  (handler-case
      (with-output-to-string (stream)
	(let ((html-template:*string-modifier* #'identity))
	  (html-template:fill-and-print-template
	   (template-path "error.tmpl")
	   (list :error-condition trace
		 :error-message   msg)
	   :stream stream)))
    (error(c) 
      (format nil "an error when generating the error page: ~A" c))))


(defun protect(f)
  (lambda()
    (handler-case 
	(funcall f)
      (error(c)
	(generate-error-page c "an error occured when generating static pages")))))



(defun serve-image-file ()
  (hunchentoot:handle-static-file (format nil "~A~A" *image-file-directory-root*
					  (hunchentoot:url-decode (hunchentoot:request-uri*)))))

(defun redirect-home ()
  (hunchentoot:redirect "/"))

(defun test-response ()
  (format nil "<html> hello world >/html>"))

(defun decode-parameter (parameter)
  (hunchentoot:url-decode (hunchentoot:get-parameter parameter)))


(defun parse-url-attribute (attribute)
  (cond 
    ((string= attribute "new") (values "archived" nil t))
    (t                        (values attribute  t   t))))

(defun query ()
  (let ((user        (hunchentoot:get-parameter "user"))
	(days-since  (parse-integer (decode-parameter "since"))))
    (multiple-value-bind  (attribute exists since) (parse-url-attribute (decode-parameter "attribute"))
      (let ((docs (nreverse (query-attribute user attribute days-since :exists exists :since since :query #'cl-mongo-find))))
	(if docs
	    (generate-page (collect-docs docs :redirect (hunchentoot:url-encode (query-url user (decode-parameter "attribute") days-since))))
	    (redirect-from-button (hunchentoot:url-encode (format nil "/statistics?user=~A" user))))))))

(defun show-cached () 
  (let ((user  (hunchentoot:get-parameter "user"))
	(url   (decode-parameter "url"))
	(image (decode-parameter "image")))
    (change-field-count user (list "url" url) "viewed") 
    (hunchentoot:handle-static-file   (format nil "~A/image/~A" *image-file-directory-root* image))))

;;  
(defun url-type->field (type)
  (cond ((string= "short" type) "url")
	((string= "full" type) "resolved-url")
	( t "url")))

(defun redirect-url ()
  (let ((url  (decode-parameter "url"))
	(type (hunchentoot:get-parameter "type"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list (url-type->field type) url) "viewed") 
    (hunchentoot:redirect url)))
;;

(defun redirect-from-button (destination)
  (hunchentoot:redirect (hunchentoot:url-decode destination)))

(defun pin ()
  (let ((url  (decode-parameter "url"))
	(redirect (decode-parameter "redirect"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list "url" url) "pinned") 
    (redirect-from-button redirect)))


(defun thumbs-up ()
  (let ((url  (decode-parameter "url"))
	(redirect (decode-parameter "redirect"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list "url" url) "liked") 
    (redirect-from-button redirect)))



(defun thumbs-down ()
  (let ((url  (decode-parameter "url"))
	(redirect (decode-parameter "redirect"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list "url" url) "disliked")
    (redirect-from-button redirect)))

(defun archive ()
  (let ((url  (decode-parameter "url"))
	(redirect (decode-parameter "redirect"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list "url" url) "archived") 
    (redirect-from-button redirect)))

(defun rearchive ()
  (let ((url  (decode-parameter "url"))
	(redirect (decode-parameter "redirect"))
	(user (hunchentoot:get-parameter "user")))
    (drop-sentiment-fields user (list "url" url)) 
    (redirect-from-button redirect)))

;;    (hunchentoot:handle-static-file *redirect-html*)))

;;    (format nil "~A => ~A" url user)))
;;    

(defun statistics ()
  (generate-statistics-page (hunchentoot:get-parameter "user")))

(defun index ()
  (generate-index-page (hunchentoot:get-parameter "user")))

(setq hunchentoot:*dispatch-table* 
      (list 
       (hunchentoot:create-regex-dispatcher "/image/"      (protect 'serve-image-file))
       (hunchentoot:create-regex-dispatcher "/buttons/"    (protect 'serve-image-file))
       (hunchentoot:create-regex-dispatcher "/redirect"    (protect 'redirect-url))
       (hunchentoot:create-regex-dispatcher "/pin"         (protect 'pin))
       (hunchentoot:create-regex-dispatcher "/thumbs-up"   (protect 'thumbs-up))
       (hunchentoot:create-regex-dispatcher "/thumbs-down" (protect 'thumbs-down))
       (hunchentoot:create-regex-dispatcher "/archive"     (protect 'archive))
       (hunchentoot:create-regex-dispatcher "/rearchive"     (protect 'rearchive))
       (hunchentoot:create-regex-dispatcher "/query"       (protect 'query))
       (hunchentoot:create-regex-dispatcher "/show-cached" (protect 'show-cached))
       (hunchentoot:create-regex-dispatcher "/statistics"  (protect 'statistics))
       (hunchentoot:create-regex-dispatcher ""             (protect 'index))))

(defun start-server ()
  (setf *url-project-server* (make-instance 'hunchentoot:acceptor :port 8080))
  (new-session-id)
  (hunchentoot:start *url-project-server*))

;;(defun stop-server()
;;  (hunchentoot:stop-server *bliky-server*)
;;  (setf *bliky-server* nil)
;;  (close-store))

(defun stop-server()
  ;;(hunchentoot:stop *bliky-server*)
  (setf *url-project-server* nil))



;;    (format nil "~A:~A:~A" user since attribute)))
;;(defun get-static-page-pathname ()
;;  (pathname (concatenate 'string (namestring (user-homedir-pathname)) "Data/twitter-project/pages/")))
;;
;;(defun static-page-path(p)
;;  (let ((name (index-unless p))) 
;;    (concatenate 'string (namestring (get-static-page-pathname)) name)))
;;
;;(defun get-static-page(p) 
;;  (with-open-file (stream (static-page-path p ) :direction :input)
;;    (slurp-stream stream)))
;;
;;(defun static-pages()
;;  (get-static-page (hunchentoot:request-uri*)))
 
#|
 
|#