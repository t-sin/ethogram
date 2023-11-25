(defpackage :ethogram
  (:use :cl)
  (:export :defspec
           :spec-desc
           :checked?
           :check))
(in-package :ethogram)

(defstruct spec
  desc
  subject
  prepare
  dispose
  (checked? nil))

(defmacro defspec (desc &key
                        subject
                        prepare
                        dispose)
  `(make-spec :desc ,desc
              :subject ,subject
              :prepare ,prepare
              :dispose ,dispose))

(defun checked? (spec)
  (spec-checked? spec))

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
