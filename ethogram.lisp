(defpackage :ethogram
  (:use :cl)
  (:export :defspec
           :checked?
           :check))
(in-package :ethogram)

(defstruct spec
  subject
  prepare
  dispose
  (prepared? nil)
  (checked? nil))

(defun defspec (subject &key
                        (prepare (lambda ()))
                        (dispose (lambda ())))
  (make-spec :subject subject
             :prepare prepare
             :dispose dispose))

(defun prepared? (spec)
  (spec-prepared? spec))

(defun checked? (spec)
  (spec-checked? spec))

(defgeneric prepare (spec))
(defmethod prepare ((spec spec))
  (setf (spec-checked? spec) nil)
  (funcall (spec-prepare spec)))

(defgeneric dispose (spec))
(defmethod dispose ((spec spec))
  (funcall (spec-dispose spec)))

(defgeneric check (spec))
(defmethod check ((spec spec))
  (prepare spec)
  (unwind-protect
       (flet ((do-nothing (c)
                (declare (ignorable c))
                (return-from check)))
         (handler-bind ((condition #'do-nothing))
           (funcall (spec-subject spec))))
    (dispose spec)
    (setf (spec-checked? spec) t)))
