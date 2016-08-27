;;;; Initialization of ASDF and other tools for
;;;; ivanp7's Common Lisp projects

(defpackage :ivanp7.init
  (:use :cl :asdf))
(in-package :ivanp7.init)

(setf *print-circle* t) ; enable circular list detection for printing

;;; ******************************************

#|
(ql:quickload :cl-fad)

(defun register-asdf-recursive-search-in-directory (directory max-depth)
  (pushnew
   #'(lambda (system)
       (let ((result nil))
         (walk-directory
          directory
          :max-depth max-depth
          :test
          #'(lambda (f)
              (let ((name (car (last (split-string (write-to-string f) #\/)))))
                (if (string= name (conc-strings system ".asd\"")) (print t) nil)))
          :action #'(lambda (f) (setf result f)))
         result))
   asdf:*system-definition-search-functions*))

(defparameter *ivanp7-systems-root-directory* #P"~/common-lisp/ivanp7/")
(defparameter *ivanp7-systems-search-max-depth* 4)

(register-asdf-recursive-search-in-directory
 *ivanp7-systems-root-directory* *ivanp7-systems-search-max-depth*)
|#
