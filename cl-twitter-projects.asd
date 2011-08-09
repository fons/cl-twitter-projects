(in-package :cl-user)

(defpackage #:cl-twitter-projects
  (:use :cl :asdf))

(in-package :cl-twitter-projects)

(defsystem cl-twitter-projects
    :name "CL-TWITTER-PROJECTS"
    :version "0.5"
    :maintainer "Fons Haffmans"
    :author "Fons Haffmans"
    :licence "LLGPL"
    :description "automation of the twitter stream"
    :components 
    ((:module "engine"
	      :serial t
	      :components ((:file "package")
			   (:file "scheduler")))
     (:module "url-project"
	      :serial t
	      :components ((:file "package")
			   (:file "url-project")
			   (:file "url-project-main")
			   (:file "web-server")
			   (:file "job-scheduler")
			   (:file "pages")
;;			   
			   ))
     )
    :depends-on (:cl-twitter :cl-twit-repl :twitter-mongodb-driver :bordeaux-threads :cl-mongo :cl-ppcre :hunchentoot :html-template))