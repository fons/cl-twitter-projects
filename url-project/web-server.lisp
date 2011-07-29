(in-package :cl-twitter-projects-url-project)

;;;---------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------
(defvar *url-project-server*  nil)
(setf hunchentoot:*message-log-pathname* "/tmp/error.file")
(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(defvar *utf-8* (flex:make-external-format :utf-8))

(setf hunchentoot:*hunchentoot-default-external-format* *utf-8*)

(defun index-unless(d)
  (if (> (length d) 1)
      d
      "index.html"))

(defun slurp-stream(stream)
  ;;from
  ;;www.emmett.ca/~sabetts/slurp.html
  ;;
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun get-template-pathname ()
  #p "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project/templates/")

(defun get-static-page-pathname ()
  #p "/Users/fons/Data/twitter-project/pages/")

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

(defun static-page-path(p)
  (let ((name (index-unless p))) 
    (concatenate 'string (namestring (get-static-page-pathname)) name)))

(defun get-static-page(p) 
  (with-open-file (stream (static-page-path p ) :direction :input)
    (slurp-stream stream)))

(defun static-pages()
  (get-static-page (hunchentoot:request-uri*)))

(defun serve-image-file ()
  (hunchentoot:handle-static-file   (format nil "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project~A" 
					    (hunchentoot:url-decode (hunchentoot:request-uri*)))))


;;(defun generate-static-pages()
;;  (let ((repo-path (create-if-missing (get-sandbox-pathname))))
;;    (push-pages-to-repo (namestring repo-path))
;;    (get-static-page "index.html")))

(defun redirect-to-google()
  (hunchentoot:redirect "http://www.google.com"))

(defun redirect-home ()
  (hunchentoot:redirect "/"))


(defun test-response ()
  (format nil "<html> hello world >/html>"))

(defun decode-parameter (parameter)
  (hunchentoot:url-decode (hunchentoot:get-parameter parameter)))

(defun show-cached () 
  (let ((user  (hunchentoot:get-parameter "user"))
	(url   (decode-parameter "url"))
	(image (decode-parameter "image")))
    (change-field-count user (list "url" url) "viewed") 
    (hunchentoot:handle-static-file   (format nil "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project/image/~A" image))))

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

(defun pin ()
  (let ((url  (decode-parameter "url"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list "url" url) "pinned") 
    (change-field-count user (list "url" url) "archived") 
    (hunchentoot:handle-static-file   (format nil "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project/templates/redirect.html"))))

(defun thumbs-up ()
  (let ((url  (decode-parameter "url"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list "url" url) "liked") 
    (change-field-count user (list "url" url) "archived") 
    (hunchentoot:handle-static-file   (format nil "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project/templates/redirect.html"))))

;;    (hunchentoot:redirect "/redirect")))

(defun thumbs-down ()
  (let ((url  (decode-parameter "url"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list "url" url) "disliked") 
    (change-field-count user (list "url" url) "archived") 
    (hunchentoot:handle-static-file   (format nil "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project/templates/redirect.html"))))
;;    (hunchentoot:redirect "/redirect")))

;;  (hunchentoot:url-decode (strip-utf8 (hunchentoot:url-encode (post-image-title doc)))))

(defun archive ()
  (let ((url  (decode-parameter "url"))
	(user (hunchentoot:get-parameter "user")))
    (change-field-count user (list "url" url) "archived") 
    (hunchentoot:handle-static-file   (format nil "/Users/fons/Repo/git.hub/cl-twitter-projects/url-project/templates/redirect.html"))))

;;    (format nil "~A => ~A" url user)))
;;    (hunchentoot:redirect "/redirect")))
  
(setq hunchentoot:*dispatch-table* 
      (list 
       (hunchentoot:create-regex-dispatcher "/image/"      (protect 'serve-image-file))
       (hunchentoot:create-regex-dispatcher "/buttons/"    (protect 'serve-image-file))
       (hunchentoot:create-regex-dispatcher "/redirect"    (protect 'redirect-url))
       (hunchentoot:create-regex-dispatcher "/pin"         (protect 'pin))
       (hunchentoot:create-regex-dispatcher "/thumbs-up"   (protect 'thumbs-up))
       (hunchentoot:create-regex-dispatcher "/thumbs-down" (protect 'thumbs-down))
       (hunchentoot:create-regex-dispatcher "/archive"     (protect 'archive))
       (hunchentoot:create-regex-dispatcher "/show-cached" (protect 'show-cached))
       (hunchentoot:create-regex-dispatcher ""             (protect 'generate-index-page))))

(defun start-server()
  (setf *url-project-server* (make-instance 'hunchentoot:acceptor :port 8080))
  (hunchentoot:start *url-project-server*))

;;(defun stop-server()
;;  (hunchentoot:stop-server *bliky-server*)
;;  (setf *bliky-server* nil)
;;  (close-store))

(defun stop-server()
  ;;(hunchentoot:stop *bliky-server*)
  (setf *url-project-server* nil))

