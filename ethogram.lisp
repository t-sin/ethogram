(defpackage :ethogram
  (:use :cl)
  (:export :defspec
           :prepared?
           :checked?
           :checking-logs
           :check))
(in-package :ethogram)

(defstruct spec
  subject
  (prepared? nil)
  (checked? nil)
  (checking-logs (list)))

(defun defspec (subject)
  (make-spec :subject subject))

(defun checking-logs (spec)
  (reverse (spec-checking-logs spec)))

(defun prepared? (spec)
  (spec-prepared? spec))

(defun checked? (spec)
  (spec-checked? spec))

(defgeneric prepare (spec))
(defmethod prepare ((spec spec))
  (setf (spec-checked? spec) nil
        (spec-prepared? spec) t)
  (push :prepare (spec-checking-logs spec)))

(defgeneric check (spec))
(defmethod check ((spec spec))
  (prepare spec)
  (setf (spec-checked? spec) t)
  (push :check (spec-checking-logs spec)))
