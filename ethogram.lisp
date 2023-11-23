(defpackage :ethogram
  (:use :cl)
  (:export :defspec
           :prepared?
           :checked?
           :check))
(in-package :ethogram)

(defstruct spec
  subject
  prepare
  (prepared? nil)
  (checked? nil))

(defun defspec (subject &key (prepare (lambda ())))
  (make-spec :subject subject
             :prepare prepare))

(defun prepared? (spec)
  (spec-prepared? spec))

(defun checked? (spec)
  (spec-checked? spec))

(defgeneric prepare (spec))
(defmethod prepare ((spec spec))
  (setf (spec-checked? spec) nil)
  (funcall (spec-prepare spec))
  (setf (spec-prepared? spec) t))

(defgeneric check (spec))
(defmethod check ((spec spec))
  (prepare spec)
  (funcall (spec-subject spec))
  (setf (spec-checked? spec) t))
