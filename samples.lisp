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

(defmacro simple-macro (&body tags)
  `(progn ,(car tags)))
   
(defun simple-fn ()
  (simple-macro (print "abc")))

(defmacro nexterly-macro (&body tags)
  (let* ((tag (caar tags))
         (tag-text (string tag))
         (element-name (make-symbol (concatenate 'string "element" tag-text))))
    `(ps
       (defun nexter-fn ()
         (let ((,element-name (chain document (create-element ,(string-downcase tag-text)))))
           ,element-name)))))
   
(defun nexterly-fn ()
  (nexterly-macro
    (tr)))

(defmacro nexter-macro (&body tags)
  `(ps
     (defun nexter-fn ()
       (let ((sample-element ,(caar tags)))
         sample-element))))
   
(defun nexter-fn ()
  (nexter-macro
    (tr)))

(defmacro next-macro (&body tags)
  `(progn ,(car tags)))
   
(defun next-fn ()
  (next-macro
    '(tr)))

(defun dynamic-list ()
  (let ((name "John"))
    (cons "name" name)))

(defmacro with-lisp-output-to-dom (&body tags)
  `(let* ((tag ,(car (car tags)))
          (element-name (concatenate 'string (symbol-to-js-string tag) "Element")))
     `(ps (let ((,element-name 123))
            ,element-name))))

(defun with-lisp-output-to-dom-fn-simple ()
  (with-lisp-output-to-dom ('tr ('td "John"))))

(defmacro with-lisp-output-to-lisp-lol (&body tags)
  `(defun hard-coded-elements ()
    (let ((sample-div (chain document (get-element-by-id "sample-div" (string ,tags)))))
      sample-div)))

(defun with-lisp-output-to-ps-fn ()
  (ps
    `(defun hard-coded-elements ()
       (let ((sample-tr
              (chain document (get-element-by-id
                               (concatenate 'string
                                            "sample" (symbol-to-js-string (car tags)))))))
         sample-tr)))
  (let ((name "John"))
    (with-lisp-output-to-ps '(tr (td name)))))

  (defmacro with-lisp-output-to-js (&body tags)
    `(ps
       (defun hard-coded-elements ()
         (let ((sample-div (chain document (get-element-by-id "sample-div" (string ,tags)))))
           sample-div))))

  (defun hard-coded-table ()
    (ps
      (defun hard-coded-table ()
        (let ((sample-div (chain document (get-element-by-id "sample-div")))
              (sample-td (chain document (create-element "td")))
              (node (chain document (create-text-node "Name: John")))
              (sample-tr (chain document (create-element "tr")))
              (sample-table (chain document (create-element "table"))))
          (chain sample-td (append-child node))
          (chain sample-tr (append-child sample-td))
          (chain sample-table (append-child sample-tr))
          (chain sample-div (append-child sample-table))
          (chain sample-td (set-attribute "color" "red;"))
          sample-div)))))))
      
(defun lisp->js-dom-fn-deep (tags &optional parent)
  (cond
    ((null tags) "")
    ((atom tags) (string tags))
    ((and (keywordp (car tags)) (stringp (cadr tags)))
     ; sampleTd.setAttribute("style", "background-color: Green;")
     (format nil "~a.setAttribute(=\"~a\", \"~a~a" (car tags) (cadr tags)
             (lisp->js-html-fn-deep (cddr tags))))
    (t
     (if (and (atom (car tags)) (not (stringp (car tags))))
         (format nil "<~a>~a</~a>"
                 (car tags)(lisp->js-html-fn-deep (cdr tags)) (car tags))
         (format nil "~a ~a"
                 (lisp->js-html-fn-deep (car tags))
                 (lisp->js-html-fn-deep (cdr tags)))))))

(defun lisp->js-html-fn-deep (tags)
  (cond
    ((null tags) "")
    ((atom tags) (string tags))
    ((and (keywordp (car tags)) (stringp (cadr tags)))
     (format nil " ~a=\"~a\" ~a" (car tags) (cadr tags)
             (lisp->js-html-fn-deep (cddr tags))))
    (t
     (if (and (atom (car tags)) (not (stringp (car tags))))
         (format nil "<~a>~a</~a>"
                 (car tags)(lisp->js-html-fn-deep (cdr tags)) (car tags))
         (format nil "~a ~a"
                 (lisp->js-html-fn-deep (car tags))
                 (lisp->js-html-fn-deep (cdr tags)))))))

(defun lisp->js-html-fn ()
  (ps
    (ps-html (:tr (:td :style "color: red;" name)))
    (ps-html ((:a :href "foobar") "blorg"))
    (who-ps-html (:a :href (generate-link) "blorg"))))

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
