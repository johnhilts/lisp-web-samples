(ql:quickload '(cl-who hunchentoot parenscript cl-json))

(defpackage :samples-webapp
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :samples-webapp)

(defun start-server (port)
  (restart-case (start (make-instance 'easy-acceptor :port port))
    (re-start-server ()
      :report "Restart Web Server"
      (stop-server *the-http-server*)
      (start-server port))))

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
         "/styles.css" "static/styles.css") *dispatch-table*))

(defvar *people-list*)

(defun start-web-app ()
  (setf *people-list* (create-test-people))
        
  (publish-static-content))

(defun get-name-list ()
  (ps
    (defun get-name-list ()
      (flet ((req-listener ()
               (let* ((names (chain -j-s-o-n (parse (@ this response-text))))
                      (serialized-names (chain names (reduce
                                                      #'(lambda (acc cur)
                                                          (let ((current-name (+ "Name: " (@ cur name) ", Age: " (@ cur age))))
                                                            (if acc
                                                                (concatenate 'string acc #\Newline current-name)
                                                                current-name))) ""))))
                 (setf (chain document (get-element-by-id "names-textarea") value) serialized-names))))

        (let ((o-req (new (-x-m-l-http-request))))
          (chain o-req (add-event-listener "load" req-listener))
          (chain o-req (open "GET" "/people"))
          (chain o-req (send)))))))

(defun in-line-javascript ()
  (ps (defun name-list-handler (evt)
        (chain evt (prevent-default))
        (get-name-list))
      (defun add-todo (evt)
        (chain evt (prevent-default))
        (setf todo (chain document (get-element-by-id "todo-content")))
        (alert (@ todo value)))
      (defun init ()
        (setf add-button (chain document
                                (get-element-by-id "todo-add-btn")))
        (chain add-button
               (add-event-listener "click" add-todo false))
        (setf name-list-button (chain document
                                      (get-element-by-id "get-names-btn")))
        (chain name-list-button
               (add-event-listener "click" name-list-handler false)))
      (setf (chain window onload) init)))

(defun make-sample-page ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title "Sample Input Form")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/styles.css")
            (:script :type "text/javascript"
                     (str (stringify
                           (get-name-list)
                           (in-line-javascript)))))
           (:body
            (:div
             (:h1 "Sample Input Form"
                  (:div
                   (:input :id "todo-check" :type "checkbox" :onclick (ps-inline (alert "You clicked the checkbox!")))
                   (:textarea :id "todo-content" :placeholder "Enter Todo info here.")
                   (:button :id "todo-add-btn" "Add")
                   )
                  (:div
                   (:h2 "Click here to get a list of names")
                   (:button :id "get-names-btn" "Get Names")
                   (:br)
                   (:textarea :id "names-textarea" :placeholder "Names Listed here" :style "height:100px;width:300px;")
                   )))))))

(define-easy-handler (sample-page :uri "/sample") ()
  (make-sample-page))

(defun get-person (name)
  (let ((person (get-person-by-name name)))
    (json:encode-json-to-string person)))

(defun get-everyone ()
  (json:encode-json-to-string (create-test-people)))
  
(define-easy-handler (sample-person :uri "/people") (name)
  (setf (content-type*) "application/json")
  (if name
      (get-person name)
      (get-everyone)))

(defun stop-server (server)
  (stop server))

(defparameter *the-http-server* (start-server 5050))

(start-web-app)
