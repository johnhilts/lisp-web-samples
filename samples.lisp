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

(defun js-html->dom-fn (tags)
  (let ((html ""))
    (labels
        ((create-element (tag random &optional (parent nil parent-supplied-p) parent-random)
           (let ((tag-text (string-downcase tag)))
             (concatenate 'string
                          "var " tag-text "Element" random " = document.createElement(\"" tag-text "\")"
                          (string #\Newline)
                          (if parent-supplied-p
                              (concatenate 'string parent parent-random ".appendChild(" tag-text "Element" random ")"
                                           (string #\Newline))
                              ""))))
         (create-text-node (tag random parent parent-random)
           (let ((tag-text (string-downcase tag)))
             (concatenate 'string
                          "var " tag-text "TextNode" random " = document.createTextNode(\"" tag-text "\")"
                          (string #\Newline)
                          parent "Element" parent-random ".appendChild(" tag-text"TextNode" random ")"
                          (string #\Newline))))         
         (js-html-fn-r (tags &optional (parent nil parent-supplied-p) parent-random)
           (let ((tag (car tags)))
             (cond
               ((null tags) "")
               ((listp tag)
                (if parent-supplied-p
                    (js-html-fn-r tag parent parent-random)
                    (js-html-fn-r tag)))
               ((atom tag)
                (let ((random (write-to-string (random 999))))
                  (if parent-supplied-p
                      (setf html (concatenate 'string html (create-element tag random parent parent-random)))
                      (setf html (concatenate 'string html (create-element tag random))))
                  (mapcar
                   #'(lambda (sub-tag)
                       (cond
                         ((keywordp sub-tag) "add-attribute")
                         ((atom sub-tag) (setf html (concatenate 'string
                                                                 html (create-text-node sub-tag random (string-downcase tag) parent-random))))
                         ((listp sub-tag) (js-html-fn-r sub-tag (concatenate 'string
                                                                             (string-downcase tag) "Element") random)))) (cdr tags))))))))
      (js-html-fn-r tags)
      html)))


(defun exercise-js-html->dom-fn ()
  (labels ((print-js-string (tags)
             (format t "~&~a =>~&~a~%" tags (js-html->dom-fn tags))))
    (print-js-string '(tr (td name) (td age)))
    (print-js-string '((td name) (td age)))
    (print-js-string '(label age))))

(defun js-html->dom-reduce-fn1 (tags)
  (labels
      ((create-open-tag (tag)
         (let ((tag-text (string-downcase tag)))
           (concatenate 'string "var " tag-text "Element = document.createElement(\"" tag-text "\")" (string #\Newline))))
       (create-text-node (acc tag)
         (let ((tag-text (string-downcase tag)))
           (concatenate 'string acc "var " tag-text "TextNode = document.createTextNode(\"" tag-text "\")" (string #\Newline))))
       (js-html-fn-r (acc cur)
         (cond
           ((keywordp cur)
            (concatenate 'string acc "element.addAttribute(\"" (string cur) "\", \"???\")" (string #\Newline)))
           ((listp cur) (concatenate 'string acc (reduce #'js-html-fn-r cur :initial-value "")))
           ((and (zerop (length acc)) (atom cur)) ; list head
            (create-open-tag cur))
           ((atom cur) (create-text-node acc cur)))))
    (reduce #'js-html-fn-r tags :initial-value "")))

(defun js-html->string-reduce-fn (tags)
  (labels
      ((create-open-tag (tag)
         (concatenate 'string "<" (string tag) ">"))
       (create-text-node (acc tag)
         (concatenate 'string acc (string tag)))
       (create-close-tag (acc tag)
         (concatenate 'string acc "</" (string tag) ">"))
       (js-html-fn-r (acc cur)
         (cond
           ((null cur) (concatenate 'string acc "</>"))
           ((keywordp cur) "add-attribute")
           ((listp cur) (concatenate 'string acc (reduce #'js-html-fn-r cur :initial-value "") "</>"))
           ((and (zerop (length acc)) (atom cur)) ; list head
            (create-open-tag cur))
           ((atom cur) (create-text-node acc cur))
           (t (create-close-tag acc cur)))))
    (reduce #'js-html-fn-r tags :initial-value "")))

(defun js-html->string-fn (tags)
  (let ((html ""))
    (labels
        ((create-open-tag (tag)
           (setf html (concatenate 'string html "<" (string tag) ">")))
         (create-text-node (tag)
           (setf html (concatenate 'string html (string tag))))
         (create-close-tag (tag)
           (setf html (concatenate 'string html "</" (string tag) ">")))
         (js-html-fn-r (tags)
           (let ((tag (car tags)))
             (cond
               ((null tags) "")
               ((listp tag) (js-html-fn-r tag))
               ((atom tag)
                (create-open-tag tag)
                (mapcar
                 #'(lambda (tag)
                     (cond
                       ((keywordp tag) "add-attribute")
                       ((atom tag) (create-text-node tag))
                       ((listp tag) (js-html-fn-r tag)))) (cdr tags))
                (create-close-tag tag))))))
      (js-html-fn-r tags)
      html)))

(defun exercise-js-html->string-fn ()
  (labels ((print-js-string (tags)
             (format t "~&~a => ~s~%" tags (js-html->string-fn tags))))
    (print-js-string '(tr (td name) (td age)))
    (print-js-string '((td name) (td age)))
    (print-js-string '(label age))))


(defun js-html-fn1 (tags)
  (let ((html ""))
    (labels
        ((js-html-fn-r (tags)
           (let ((tag (car tags)))
             (cond
               ((null tags) "")
               ((and (listp tags) (atom tag) (null (cdr tags)))
                (concatenate 'string html (string tag)))
               ((and (listp tags) (atom tag) (listp (cdr tags)))
                (concatenate 'string html "<" (string tag) ">" (js-html-fn-r (cdr tags)) "</" (string tag) ">"))
               ((and (listp tags) (listp tag))
                (js-html-fn-r tag))
               (t
                (concatenate 'string html (string tag)))))))
      (js-html-fn-r tags))))

(defmacro js-html (&body tags)
  "this has to use document.createElement() or whatever the DOM function is"
  `(progn
     ,@(do ((tag-head tags (cdr (car tag-head)))
            (html "")
            (res ()))
           ((null tag-head) res)
         (push `(setf html (concatenate 'string ,html (string ,(car (car tag-head))))) res))))
  
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
                  (:div :id "sample-div"
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
