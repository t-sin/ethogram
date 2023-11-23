(defpackage :ethogram
  (:use :cl)
  (:export :defspec
           :spec-name
           :checked?
           :check))
(in-package :ethogram)

(defstruct spec
  subject
  subject-name
  prepare
  dispose
  (checked? nil))

(defmacro defspec (subject &key
                           prepare
                           dispose)
  `(make-spec :subject ,subject
              :subject-name ,(symbol-name (second subject))
              :prepare ,prepare
              :dispose ,dispose))

(defun checked? (spec)
  (spec-checked? spec))

(defgeneric spec-name (spec))
(defmethod spec-name ((spec spec))
  (format nil "~a: a function" (spec-subject-name spec)))

(defgeneric prepare (spec))
(defmethod prepare ((spec spec))
  (setf (spec-checked? spec) nil)
  (unless (null (spec-prepare spec))
    (funcall (spec-prepare spec))))

(defgeneric dispose (spec))
(defmethod dispose ((spec spec))
  (unless (null (spec-dispose spec))
    (funcall (spec-dispose spec))))

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
