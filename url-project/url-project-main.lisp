(in-package :cl-twitter-projects-url-project)

(defun initialize-twitter (user)
  (progn
    (cl-twit-repl:get-authenticated-user user)
    (cl-twitter:use-cache)
    (cl-twitter:use-db :twitter-mongodb-driver)
    (cl-twitter:db-status)))

(defun next-batch (screen-name)
  ;;(initialize-twitter screen-name)
  (cl-twitter:collect-friends-timeline)
  (cl-twitter:collect-home-timeline)
  (let ((count (process-latest-friend-urls screen-name)))
    (add-missing-timestamp screen-name :limit count)
    (resolve-urls screen-name :limit count)
    (get-screen-shots screen-name :limit count))) 

(defun job-next-batch (screen-name)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" screen-name))
  (next-batch screen-name))

(defun monitor-next-batch (screen-name)
  (initialize-twitter screen-name)
  (submit-job "monitor-next-batch" #'job-next-batch :args (list screen-name) :every 300 :iter 2000 :errorhandler t))

