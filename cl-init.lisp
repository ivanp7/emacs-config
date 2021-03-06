;;;; Initialization of Common Lisp ASDF, Quicklisp and other tools and settings

#-asdf (require :asdf) ;; Needed for some CL implementations to see ASDF package

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(setf slynk:*slynk-pprint-bindings* nil)

(setf *print-circle* t) ; enable circular list detection for printing
(setf *read-default-float-format* 'double-float)

#|
(defpackage #:cl-init
  (:use #:cl #:asdf))
(in-package #:cl-init)
|#
