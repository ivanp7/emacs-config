;;;; Initialization of ASDF and other tools for
;;;; ivanp7's Common Lisp projects

(defpackage :ivanp7.init
  (:use :cl :asdf))
(in-package :ivanp7.init)

(setf *print-circle* t) ; enable circular list detection for printing

(defun register-asdf-recursive-search-in-directory (directory max-depth)
  (labels
      ;; Filelib from PCL
      ((component-present-p (value)
         (and value (not (eql value :unspecific))))
       (directory-pathname-p (p)
         (and
          (not (component-present-p (pathname-name p)))
          (not (component-present-p (pathname-type p)))
          p))
       (pathname-as-directory (name)
         (let ((pathname (pathname name)))
           (when (wild-pathname-p pathname)
             (error "Can't reliably convert wild pathnames."))
           (if (not (directory-pathname-p name))
               (make-pathname
                :directory (append (or (pathname-directory pathname)
                                      (list :relative))
                                   (list (file-namestring pathname)))
                :name nil
                :type nil
                :defaults pathname)
               pathname)))
       (directory-wildcard (dirname)
         (make-pathname
          :name :wild
          :type #-clisp :wild #+clisp nil
          :defaults (pathname-as-directory dirname)))
       #+clisp
       (clisp-subdirectories-wildcard (wildcard)
         (make-pathname
          :directory (append (pathname-directory wildcard) (list :wild))
          :name nil
          :type nil
          :defaults wildcard))
       (list-directory (dirname)
         (when (wild-pathname-p dirname)
           (error "Can only list concrete directory names."))
         (let ((wildcard (directory-wildcard dirname)))
           #+(or sbcl cmu lispworks)
           (directory wildcard)
           #+openmcl
           (directory wildcard :directories t)
           #+allegro
           (directory wildcard :directories-are-files nil)
           #+clisp
           (nconc
            (directory wildcard)
            (directory (clisp-subdirectories-wildcard wildcard)))
           #-(or sbcl cmu lispworks openmcl allegro clisp)
           (error "list-directory not implemented")))
       (walk-directory (dirname &key (action #'print) (test (constantly t))
                                (max-depth -2))
         (labels
             ((walk (name depth)
                (cond
                  ((directory-pathname-p name)
                   (unless (= depth max-depth)
                     (dolist (x (list-directory name)) (walk x (1+ depth)))))
                  ((funcall test name) (funcall action name)))))
           (walk (pathname-as-directory dirname) -1)))
       ;; functions from lisper.ru
       (+. (arg-0 &rest args)
         (cond
           ((stringp arg-0) (with-output-to-string (result)
                              (princ arg-0 result)
                              (dolist (arg args) (princ arg result))))
           ((symbolp arg-0) (values (intern (apply #'+. "" arg-0 args))))
           ((pathnamep arg-0) (merge-pathnames
                               arg-0
                               (with-output-to-string (str)
                                 (dolist (arg args) (princ arg str)))))
           (t nil)))
       (split-string (string char)
         (loop for i = 0 then (1+ j)
            as j = (position char string :start i)
            collect (subseq string i j)
            while j)))
    ;; labels body starts here
    (pushnew
     #'(lambda (system)
         (let ((result nil))
           (walk-directory
            directory
            :max-depth max-depth
            :test
            #'(lambda (f)
                (let ((name (car (last (split-string (write-to-string f) #\/)))))
                  (if (string= name (+. system ".asd\"")) (print t) nil)))
            :action #'(lambda (f) (setf result f)))
           result))
     asdf:*system-definition-search-functions*)))

(defparameter *ivanp7-systems-root-directory* #P"~/lisp/ivanp7/")
(defparameter *ivanp7-systems-search-max-depth* 2)

(register-asdf-recursive-search-in-directory
 *ivanp7-systems-root-directory* *ivanp7-systems-search-max-depth*)
