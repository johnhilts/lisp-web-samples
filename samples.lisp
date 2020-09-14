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

(defmacro run-code (code)
  `(progn
     ,code))

;;* Syntax Idea
'(jsx-macro ; added quote so it's not compiled
 (tr (td style "color:green;" (ps (alert "hello!"))))) ; only insert parenscript??

(ps
  (defmacro eval-some-js (elements)
    (labels
        ((parse-out-js (string)
           (read-from-string
            (subseq string
                    (+ 2 (search "{{" string))
                    (search "}}" string))))
         (eval-some-js-r (elements)
           (if (atom elements)
               `(chain console (log ,elements))
               (mapcar
                #'(lambda (e)
                    (cond
                      ((and (typep e 'string) (zerop (search "{{" e)))
                       `(progn
                          ,(parse-out-js e)))
                      ((listp e)
                       (cons (eval-some-js-r (car e)) (eval-some-js-r (cdr e))))
                      (t `(chain console (log ,e))))) elements))))
      (eval-some-js-r elements))))

  (defun test-eval-some-js ()
    (ps
      (defun some-function ()
        (eval-some-js
         (tr (td "{{(alert \"test123\")}}"))))))

;; transform this: "(update-todo (chain index (to-string)))"
;; into this: (chain parent-elment-id (add-event-listener "click" (chain update-todo (bind null index)) false))
;; transform this: "(fun-name param1 ... paramN)"
;; into this: (chain parent-elment-id (add-event-listener "click" (chain fun-name (bind null param1 paramN)) false))
(ps
  (defmacro parse-some-parenscript (event-name string)
    (let* ((expression (read-from-string string))
           (fun-name (car expression))
           (parameters (cdr expression))
           (event-attribute-key (subseq (symbol-to-js-string event-name) 2)))
      `(chain parent-elment-id (add-event-listener ,event-attribute-key (chain ,fun-name (bind null ,@parameters)) false)))))

(defun test-parse-some-parenscript ()
  (ps (parse-some-parenscript onclick "(update-todo (chain index (to-string)))")))

  (defun lisp->ps (html-elements)
    "Process each Lisp element and pass it to an appropriate parenscript function"
    (let ((tag (car html-elements)))
      (cond
        ((null html-elements) "")
        ((cons-pair-p html-elements) (format t "~&Looking at attribute: ~a~%" html-elements))
        (t
         (format t "~&createElement(~s)~%" (string tag))
         (format t "macro output: ~a" (process-tag-experiment tag))
         (mapcar
          #'(lambda (element)
              (format t "~&Looking at: ~a~%" element)
              (cond
                ((stringp element) (format t "createTextNode(~s)~%append to parent: ~a~&" element tag))
                ((listp element) (lisp->ps element))))
          (cdr html-elements))))))

  (defmacro with-lisp-output-ps-experiment (&body html)
    "turn form into a list; pass it to functions to operate on"
    `(lisp->ps ',@html))

  (defun test-with-lisp-output-ps-experiment ()
    (with-lisp-output-ps-experiment
        (tr (td (style . "color-green;") "John"))))


  (defun cons-pair-p (possible-cons)
    (and (consp possible-cons) (atom (cdr possible-cons)) (not (null (cdr possible-cons)))))

  (ps
    (defmacro process-tag-set-attribute-experiment (element-tag attribute)
      (format t "~&Looking at attribute elements; tag: ~a, attribute: ~a" element-tag attribute)
      (let* ((tag (string-downcase element-tag))
             (attribute-key (string (caadr attribute)))
             (attribute-value (cdadr attribute)))
        `(ps
           (defun process-tag-set-attribute (element-tag attribute-key attribute-value)
             (let ((element (chain document (get-element-by-id element-tag))))
               (chain element (set-attribute attribute-key attribute-value))
               element))
           (process-tag-set-attribute ,tag ,attribute-key ,attribute-value)))))


  (defmacro process-tag-set-attribute-experiment (element-tag attribute)
    (format t "~&Looking at attribute elements; tag: ~a, attribute: ~a" element-tag attribute)
    (let* ((tag (string-downcase element-tag))
           (attribute-key (string (caadr attribute)))
           (attribute-value (cdadr attribute)))
      `(ps (defun process-tag-set-attribute (element-tag attribute-key attribute-value)
             (let ((element (chain document (get-element-by-id element-tag))))
               (chain element (set-attribute attribute-key attribute-value))
               element))
           (process-tag-set-attribute ,tag ,attribute-key ,attribute-value))))


  (defun test-process-tag-set-attribute-experiment ()
    (let ((tag (string-downcase 'tr))
          (attribute '(style . "color:purple;")))
      (process-tag-set-attribute-experiment tag (list attribute))))

  (defmacro interpolate-html-elements-from-lisp-form (&body elements)
    (format t "~&elements: ~a~%" elements)
    (labels
        ((parse-it (element)
           (format t "~&element: ~a~%" element)
           (cond
             ((cons-pair-p element)
              (list (string-downcase (car element)) (cdr element)))
             (t element))))
      `(let ((parsed-elements ,@(mapcar #'parse-it (car elements))))
         parsed-elements)))

  (defun test-interpolate-html-elements-from-lisp-form ()
    (interpolate-html-elements-from-lisp-form
     (ps (tr (td (style . "color:purple;") "John")))))

  (ps
    (defmacro test-ps-macro (tags)
      "This works pretty well, but doesn't do the recursion logic as well for the parent element
Use (test-the-ps-macro) to call it, or check the output in the repl from the formats when compiling (test-the-ps-macro) "
      (format t "~&*** starting with ~a ***~%" tags)
      (labels
          ((parse-it (e)
             (let* ((tag e)
                    (parse-it-r
                     #'(lambda (e)
                         (format t "~&Looking at elements in ~a~%" tag)
                         (cond
                           ((null e) "")
                           ((stringp e)
                            (format t "~&~s is a string" e)
                            (string e))
                           ((cons-pair-p e)
                            (format t "~&Looking at attribute: ~a~%" e)
                            ;;(format t "~&Output from attribute-set: ~a~%" (process-tag-set-attribute-experiment tag (list (list e)))))
                            (format t "~&Output from attribute-set: ~s~%" `(ps (set-an-attribute ,tag ,(string (car e)) ,(string (cdr e)))))
                            (format t "~&tag: ~a, (car e): ~s, (cdr e): ~s~%" tag (string (car e)) (string (cdr e))))
                           ((atom e)
                            (format t "~&~a is an atom" e)
                            (string e))
                           ((listp e)
                            (format t "~&~a is a list (make recursive call)" e)
                            (parse-it (car e))
                            (parse-it (cdr e)))))))
               (funcall parse-it-r e))))
        `(let ((parsed-tags ,@(mapcar #'parse-it tags)))
           parsed-tags))))
  
  (defun test-the-ps-macro ()
    (ps
      (test-ps-macro
       (tr (td (style . "color:green;") "John") (td "Bill")))
      (test-ps-macro
       (td (style . "color: red;") "John")))))

(defmacro simple-macro (tag)
  (let* ((element-tag `(string-downcase ,tag))
        (ps-commands 
         `(
           (defun process-tag (element-tag)
             (let ((element (chain document (create-element element-tag))))
               (chain element (set-attribute "id" (concatenate 'string element-tag "1234")))
               element))
           (process-tag ,element-tag))))
    `(ps ,@ps-commands)))
    
(defpsmacro process-tag-experiment (tag)
  (let ((element-tag (string-downcase tag)))
    (format t "~&inside macro: ~a~%" element-tag)
    `(ps
       (defun process-tag (element-tag)
         (let ((element (chain document (create-element element-tag))))
           (chain element (set-attribute "id" (concatenate 'string element-tag "1234")))
           element))
       (process-tag ,element-tag))))

(defmacro process-tag-set-attribute-experiment (element-tag attribute)
  (let* ((tag (string-downcase (cadr element-tag)))
         (attribute-key (string (caadr attribute)))
         (attribute-value (cdadr attribute)))
    `(ps
       (defun process-tag-set-attribute (element-tag attribute-key attribute-value)
         (let ((element (chain document (get-element-by-id element-tag))))
           (chain element (set-attribute attribute-key attribute-value))
           element))
       (process-tag-set-attribute ,tag ,attribute-key ,attribute-value))))

(defun process-tag-set-attribute-experiment-fn (element-tag attribute)
  (let* ((tag (string-downcase element-tag))
         (attribute-key (string (car attribute)))
         (attribute-value (cdr attribute)))
    (list tag attribute-key attribute-value)))

(defun test-process-tag-set-attribute-experiment ()
  (let* ((element '(td (style . "color:green;") "John"))
         (tag (car element))
         (attribute (cadr element)))
    (process-tag-set-attribute-experiment tag attribute)))

(defmacro attribute-experiment-no-parsing (element-tag attribute-key attribute-value)
  `(ps
     (defun process-tag-set-attribute (element-tag attribute-key attribute-value)
       (let ((element (chain document (get-element-by-id element-tag))))
         (chain element (set-attribute attribute-key attribute-value))
         element))
     (process-tag-set-attribute ,element-tag ,attribute-key ,attribute-value)))
;  `(ps  "~a.setAttribute(~s ~s)" ,tag ,attribute-key ,attribute-value))

(defun test-attribute-experiment-no-parsing ()
  (attribute-experiment-no-parsing "td" "style" "color:black;"))

(defmacro attribute-experiment (&body element)
  (let* ((tag (string (car (car element)))))
  `(format t "~a" ,tag)))

(defun test-attribute-experiment ()
  (attribute-experiment (td (style . "color:green;") "John")))

(defun cons-pair-p1 (possible-cons)
  (and (consp possible-cons) (atom (cdr possible-cons))))

(defun cons-pair-p (possible-cons)
  (or
   (and (consp possible-cons) (atom (cdr possible-cons)))
   (and (consp possible-cons) (listp (cdr possible-cons)) (not (and (= 1 (length (cdr possible-cons))) (listp (car (cdr possible-cons))))))))

(defun test-cons-pair-p ()
  "all these should be true"
  (and (cons-pair-p '(1 . 2))
(cons-pair-p '(1 . "Test"))
(cons-pair-p '(1 . (alert "123")))
(cons-pair-p '(1 . (some-function)))
))

(defmacro process-tag-map-experiment-elements-only (element)
  "**ELEMENTS ONLY VERSION** This has the recursion logic down well!
Test it by just calling it: (test-process-tag-map-experiment-macro)"
  (labels
      ((process-tag-r (element) ;; make an optional parent tag parameter
         (let ((tag (car element)))
           `(append (list ,(string tag))
                    ,@(mapcar
                    #'(lambda (e)
                        (cond
                          ((cons-pair-p e)
                           (let ((attribute-key (string (car e)))
                                 (attribute-value (string (cdr e))))
                             `(list ,attribute-key ,attribute-value)))
                          ((stringp e)
                           `(list ,e))
                          ((listp e)
                           (process-tag-r e)
                           )))
                    (cdr element)))
           )))
    (process-tag-r element)))

(defun test-process-tag-map-experiment-elements-only ()
  (list (process-tag-map-experiment-elements-only
   (tr (td (style . "color:green;") "John")  (td "Bill")))))

#||
"function createElements(parentElement) {
    var trElement123 = createAnElement(parentElement, 'tr');
    var tdElement456 = createAnElement(trElement123, 'td');
    setAnAttribute(tdElement456, 'STYLE', 'color:green;');
    setTextNode(tdElement456, 'John');
    var tdElement789 = createAnElement(trElement123, 'td');
    setTextNode(tdElement789, 'Bill');
    return trElement123
};"
||#

(defmacro with-very-hard-coded-version (element)
  (ps
    (defun create-elements (parent-element)
      (let ((trElement (create-an-element parent-element "tr")))
        parent-element))))

(defmacro with-hard-coded-version (element)
  `(progn
     ,@(list
        `(ps
          (defun create-elements (parent-element)
            (let* ((tr-element (create-an-element parent-element ,(string (car element))))
                   (td-element (create-an-element tr-element ,(string (caadr element)))))
              (set-attribute td-element ,(string-downcase (car (cadadr element))) ,(cdr (cadadr element)))
              (set-text-node td-element ,(car (cddadr element)))
              parent-element))))))

(defmacro with-hard-coded-version-faux-recursion (element)
  (labels ((some-local-function (sub-element)
             `(defun create-more-elements (parent-element)
                (let ((td-element (create-an-element parent-element ,(string (caadr element)))))
                  (set-text-node td-element ,sub-element)
                  td-element))))
    `(progn
       ,@(list
          `(ps
             (defun create-elements (parent-element)
               (let* ((tr-element (create-an-element parent-element ,(string (car element))))
                      (td-element (create-an-element tr-element ,(string (caadr element)))))
                 (set-attribute td-element ,(string-downcase (car (cadadr element))) ,(cdr (cadadr element)))
                 (set-text-node td-element ,(car (cddadr element)))
                 ,(some-local-function (car (cdaddr element)))
                 parent-element)))))))

(defun define-ps-swap ()
(ps
  (defmacro swap (a b)
    (with-ps-gensyms (tmp)
      `(let ((,tmp ,a))
         (setf ,a ,b)
         (setf ,b ,tmp))))))

(defun test-ps-swap ()
  (ps
    (swap (@ element-a style display) (@ element-b style display))))

(defun define-ps-with-html-macro1 ()
  (ps
    (defmacro with-html-elements1 (elements)
      `(chain console (log ,(car elements))))))

(defun test-ps-with-html-macro1 ()
  (ps
    (defun some-function ()
      (let ((list ([] "abc" "def" "ghi")))
        (chain list (map #'(lambda (item)
                             (with-html-elements1 (tr (td (style . "color:blue;") "John") (td "Bill"))))))))))

(defun define-ps-with-html-macro ()
  (ps
    (defmacro with-html-elements (elements)
      (labels
          ((process-tag-r (element &optional (parent nil parent-supplied-p))
             (let* ((tag (car element))
                    (parent-element (gensym (concatenate 'string (string-downcase tag) "Element")))
                    (parent-element-parameter (if parent-supplied-p parent (make-symbol "parent-element"))))
               (cons
                `(let ((,parent-element (create-an-element ,parent-element-parameter ,(string tag)))))
                (mapcar
                 #'(lambda (e)
                     (cond
                       ((cons-pair-p e)
                        `(set-an-attribute ,parent-element ,(string (car e))  ,(string (cdr e))))
                       ((stringp e)
                        `(set-text-node ,parent-element ,e))
                       ((listp e)
                        `(progn
                           ,@(process-tag-r e parent-element)))
                       ((symbolp e)
                        `(set-text-node ,parent-element ,e))))
                 (cdr element))))))
        `(progn ,@(process-tag-r elements))))))

(defun test-ps-with-html-macro ()
  (ps
    (defun some-function ()
      (let ((list ([] "abc" "def" "ghi")))
        (chain list (map #'(lambda (item)
                             (with-html-elements
                                 (tr
                                  (td (style . "color:blue;") item)
                                  (td "Bill"))))))))))

(defmacro with-inline-html-elements (elements)
  (labels
      ((process-tag-r (element &optional (parent nil parent-supplied-p))
         (let* ((tag (car element))
                (parent-element (gensym (concatenate 'string (string-downcase tag) "Element")))
                (parent-element-parameter (if parent-supplied-p parent (make-symbol "parent-element"))))
           (cons
            `(let ((,parent-element (create-an-element ,parent-element-parameter ,(string tag)))))
            (mapcar
             #'(lambda (e)
                 (cond
                   ((cons-pair-p e)
                    `(set-an-attribute ,parent-element ,(string (car e))  ,(string (cdr e))))
                   ((stringp e)
                    `(set-text-node ,parent-element ,e))
                   ((listp e)
                    `(progn
                       ,@(process-tag-r e parent-element)))))
             (cdr element))))))
    `(ps
       (let ((parent-element (chain document (get-element-by-id "parent123"))))
         ,@(process-tag-r elements)
         parent-element))))

(defun test-with-inline-html-elements ()
  (with-inline-html-elements
      (tr (td (style . "color:green;") "John") (td (onclick . "Testclickhandler()") "Bill"))))

(import-macros-from-lisp 'with-html-elements)

(defun test-with-inline-html-elements-in-ps ()
  (ps
    (defun some-function ()
      (let ((some-list ([] 1 2 3)))
        (chain some-list (map #'(lambda (item)
                                  (with-inline-html-elements (tr (td item))))))))))

(defmacro with-html-elements (elements)
"
input: html elements as sexprs
output: javascript that creates the elements in DOM based on the sexprs
"
(labels
    ((process-tag-r (element &optional (parent nil parent-supplied-p))
       (let* ((tag (car element))
              (parent-element (gensym (concatenate 'string (string-downcase tag) "Element")))
              (parent-element-parameter (if parent-supplied-p parent (make-symbol "parent-element"))))
         (cons
          `(let ((,parent-element (create-an-element ,parent-element-parameter ,(string tag)))))
          (mapcar
           #'(lambda (e)
               (cond
                 ((cons-pair-p e)
                  `(set-an-attribute ,parent-element ,(string (car e))  ,(string (cdr e))))
                 ((stringp e)
                  `(set-text-node ,parent-element ,e))
                 ((listp e)
                  `(progn
                     ,@(process-tag-r e parent-element)))))
           (cdr element))))))
  `(ps
     (defun create-elements (parent-element)
       ,@(process-tag-r elements)
       parent-element)
     (let ((parent-element (chain document (get-element-by-id "parent123"))))
       (create-elements parent-element)))))

(defun test-with-html-elements ()
  (with-html-elements
      (table (tr (td (style . "color:green;") "John") (td (onclick . "testClickHandler()") "Bill")))))

(defun test-with-hard-coded-version-faux-recursion ()
  (with-hard-coded-version-faux-recursion
      (tr (td (style . "color:green;") "John") (td "Bill"))))

(defmacro with-preserved-casing (var)
  `(let ((symbol-with-preserved-casing (make-symbol (string ,(car var)))))
     symbol-with-preserved-casing))

(defun test-with-hard-coded-version ()
  (with-hard-coded-version
      (tr (td (style . "color:green;") "John") (td "Bill"))))

(defmacro process-tag-map-experiment-macro1 (element)
  "**MACRO VERSION** This has the recursion logic down well!
Test it by just calling it: (test-process-tag-map-experiment-macro)"
  (labels
      ((process-tag-r (element) ;; make an optional parent tag parameter
         (let ((tag (car element)))
           (append (list `(let ((element-tag ,(string tag)))
                      (create-an-element parent-element element-tag)))
                   (mapcar
                    #'(lambda (e)
                        (cond
                          ((cons-pair-p e)
                           `(let ((attribute-key ,(string (car e)))
                                  (attribute-value ,(string (cdr e))))
                              (set-an-attribute parent-element attribute-key attribute-value)))
                          ((stringp e)
                           `(let ((text ,e))
                              (set-text-node parent-element text)))
                          ((listp e)
                           (process-tag-r e))))
                    (cdr element))))))
    (append '(ps) (process-tag-r element))))

(defun test-process-tag-map-experiment-macro ()
  (process-tag-map-experiment-macro1
   (tr (td (style . "color:green;") "John") (td "Bill"))))

(defun process-tag-map-experiment ()
  "This has the recursion logic down well!
Test it by just calling it: (process-tag-map-experiment)"
  (labels
      ((process-tag-r (element)
         (let ((tag (car element)))
           (format t "~&createElement(~s)" (string tag))
           (progn
             ;(process-tag-experiment (list element))
           (mapcar
            #'(lambda (e)
                (cond
                  ((cons-pair-p e)
                   (format t "~&~a.setAttribute(~s, ~s)~%" tag (string (car e)) (string (cdr e))))
                   ;(process-tag-set-attribute-experiment (list tag) (list (list e))))
                  ((stringp e) (format t "createTextNode(~s)~%append to parent: ~a~&" e tag))
                  ((listp e)
                   (format t "~&recursive call with ~a++~&~a.appendChild(~a)...~&" e tag e)
                   (process-tag-r e)
                   (format t "~&~a.appendChild(~a)~%" tag e))))
            (cdr element)))
           )))
    (let ((element '(tr (td (style . "color:green;") "John"))))
      (process-tag-r element))))

(defun test-process-tag-experiment ()
  (ps (process-tag-experiment (td))))

(defun process-element (element)
  (let ((tag (car element)))
    (macrolet
        ((append-child-element (tag child-element)
                                        ;`(ps
           `(chain ,tag (append-child ,child-element))))
      (mapcar
       #'(lambda (element-part)
           (cond
             ((listp element-part)
              (let ((child-element (process-element element-part))
                    (some-js (append-child-element tag child-element)))
                (ps some-js)))
             ((symbolp element-part)
                                        ;`(ps
              `(chain document (create-element ,element-part)));)
             ((equal 'ps element-part) `(ps (cdr ,element-part))) ; assumes ps was detected as a list so this is now the "ps element"
             #||
             ((stringp element-part)
                                        ; same as above ... + could be attribute value
              `(append-child-element ,tag; `(ps
                                     (ps (chain document (create-text-node ,element-part))))
              )
||#
))
       element))))

  (defmacro lisp->js&html-macro-deep (&body tags)
    (cond
      ((null tags) ())
      ((and (keywordp (car tags)) (stringp (cadr tags)))
       (list (car tags) (cadr tags)))
      ;; need to go through the rest of the list: (lisp->js-html-fn-deep (cddr tags))))
      ;; ((atom tags) (string tags))
      (t
       (if (and (atom (car tags)) (not (stringp (car tags))))
           (let* ((tag (caar tags))
                  (tag-text (string tag))
                  (element-name (make-symbol (concatenate 'string "element" tag-text))))
             `(ps (let ((,element-name (chain document (create-element (car tags)))))
                    ,element-name)))
           ;; need to keep going: (lisp->js-html-fn-deep (cdr tags)) (car tags))
           (progn
             `(ps (lisp->js&html-fn-deep (car tags))
                  (lisp->js&html-fn-deep (cdr tags))))))))

(defun lisp->js&html-fn-deep (tags)
  (cond
    ((null tags) ())
    ((atom tags)
     (string tags))
    ((and (keywordp (car tags)) (stringp (cadr tags)))
     (list (car tags) (cadr tags)))
    ;; need to go through the rest of the list: (lisp->js-html-fn-deep (cddr tags))))
    (t
     (if (and (atom (car tags)) (not (stringp (car tags))))
         (ps (let ((sample-element (chain document (create-element (car tags)))))
               sampleElement))
         ;; need to keep going: (lisp->js-html-fn-deep (cdr tags)) (car tags))
         (progn
           (lisp->js&html-fn-deep (car tags))
           (lisp->js&html-fn-deep (cdr tags)))))))

(defun lisp->ps->js-with-html (people)
  (cond
    ((null people) nil)
    (t
     (with-list-output-ps-create-element
         "td")
;;     (format t "~a~%" (car people))
;;     (lisp->ps->js-with-html (cdr people))
     )))

(defmacro with-list-output-ps-create-element (tag)
  (let* ((tag-text (string tag))
         (element-name (make-symbol (concatenate 'string "element" tag-text)))
         (function-name (make-symbol (concatenate 'string "function" tag-text))))
    `(ps (defun ,function-name ()
           (let ((,element-name (chain document (create-element ,(string-downcase tag-text)))))
             ,element-name)))))

(defun test-lisp->ps-dhtml ()
  (lisp->ps->js-with-html '(John Masami Matthew Mark)))

(defmacro nexterly-use-data-macro (data)
  `(ps
     (defun nexter-fn ()
       (alert ,(format nil "Hello, ~a!" data)))))

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

(defmacro simple-macro (&body tags)
  `(progn ,(car tags)))
   
(defun simple-fn ()
  (simple-macro (print "abc")))

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

  (defmacro with-lisp-output-to-js (&body tags)
    `(ps
       (defun hard-coded-elements ()
         (let ((sample-div (chain document (get-element-by-id "sample-div" (string ,tags)))))
           sample-div))))
      
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

(defun js-html->dom-reduce-fn (tags)
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
                 (setf (chain document (get-element-by-id "names-textarea") value) serialized-names)
                 (render-name-list names))))

        (let ((o-req (new (-x-m-l-http-request))))
          (chain o-req (add-event-listener "load" req-listener))
          (chain o-req (open "GET" "/people"))
          (chain o-req (send)))))))

(defmacro+ps render-name-list->js&html-table (list-name)
  (let ((table-name (make-symbol (concatenate 'string list-name "-table")))
        (row-name (make-symbol (concatenate 'string list-name "-row")))
        (cell-name (make-symbol (concatenate 'string list-name "-cell")))
        (text-node-name (make-symbol (concatenate 'string list-name "-text-node"))))
;;    `(ps
    `(defun render-name-list (names)
         (chain names (map
                       #'(lambda (name)
                           (let ((,table-name (chain document (get-element-by-id "sample-table")))
                                 (,row-name (chain document (create-element "tr")))
                                 (,cell-name (chain document (create-element "td")))
                                 (,text-node-name (chain document (create-text-node (@ name name)))))
                             (chain ,cell-name (append-child ,text-node-name))
                             (chain ,row-name (append-child ,cell-name))
                             (chain ,table-name (append-child ,row-name)))))))));)

(defun render-name-list ()
  (ps (render-name-list->js&html-table "my-names")))

(defun render-name-list1 ()
  (ps
    (defun render-name-list (names)
      (chain names (map
                    #'(lambda (name)
                        (let ((sample-table (chain document (get-element-by-id "sample-table")))
                              (sample-tr (chain document (create-element "tr")))
                              (sample-td (chain document (create-element "td")))
                              (sample-node (chain document (create-text-node (@ name name)))))
                          (chain sample-td (append-child sample-node))
                          (chain sample-tr (append-child sample-td))
                          (chain sample-table (append-child sample-tr)))))))))

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
                           (nexterly-fn)
                           (render-name-list)
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
                   (:table :id "sample-table")
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
