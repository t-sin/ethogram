(defpackage :ethogram
  (:use :cl)
  (:export :defspec
           :prepared?
           :checked?
           :check))
(in-package :ethogram)

(defstruct spec
  subject
  (prepared? nil)
  (checked? nil))

(defun defspec (subject)
  (make-spec :subject subject))

(defun prepared? (spec)
  (spec-prepared? spec))

(defun checked? (spec)
  (spec-checked? spec))

(defgeneric prepare (spec))
(defmethod prepare ((spec spec))
  (setf (spec-checked? spec) nil
        (spec-prepared? spec) t))

(defgeneric check (spec))
(defmethod check ((spec spec))
  (prepare spec)
  (setf (spec-checked? spec) t))
