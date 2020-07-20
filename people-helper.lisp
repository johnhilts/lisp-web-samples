(in-package :samples-webapp)

;; add dto for people here and some helper methods
(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (age
    :initarg :age
    :initform 0
    :accessor age)
   ))

(defun create-test-people ()
  (list
   (make-instance 'person :name "John" :age 51)
   (make-instance 'person :name "Masami" :age 56)
   (make-instance 'person :name "Matt" :age 28)
   (make-instance 'person :name "Mark" :age 27)))

(defun display-1-person-hard-coded ()
  (let* ((some-test-people (create-test-people))
         (1-person (car some-test-people)))
    (format t "Name:~a, Age:~d"
            (name 1-person) (age 1-person))))

(defun display-all-test-people ()
  (let* ((some-test-people (create-test-people)))
    (dolist (person some-test-people)
      (format t "Name:~a, Age:~d~%"
              (name person) (age person)))))

(defun display-person-by-name (search-name)
  (let* ((some-test-people (create-test-people))
         (the-person (find-if #'(lambda (a-person)
                            (string= search-name (name a-person))) some-test-people)))
    (if the-person
        (format t "Name:~a, Age:~d"
                (name the-person) (age the-person))
        (format t "Can't find the person!"))))

(defun get-person-by-name (search-name)
  (let ((some-test-people (create-test-people)))
    (find-if #'(lambda (a-person)
                 (string= search-name (name a-person))) some-test-people)))


