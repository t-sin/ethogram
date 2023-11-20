(defpackage :ethogram
  (:use :cl)
  (:export :defspec
           :checked?
           :check))
(in-package :ethogram)

(defstruct spec
  subject
  (checked? nil))

(defun defspec (subject)
  (make-spec :subject subject))

(defun checked? (spec)
  (spec-checked? spec))

(defun check (spec)
  (setf (spec-checked? spec) t))
