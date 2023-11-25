(defpackage :ethogram
  (:use :cl)
  (:export
   ;; spec
   :defspec
   :spec-desc
   ;; example
   :examples
   :function-examples
   :function-examples-input
   :function-examples-output
   ;; methods
   :checked?
   :check))
(in-package :ethogram)

(defstruct function-examples
  input output)

(defun parse-function-examples (body)
  (values (getf body :for)
          (getf body :returns)))

(defmacro examples (type &body body)
  (assert (eq type :function))
  (multiple-value-bind (input output)
      (parse-function-examples body)
    `(make-function-examples :input ',input
                             :output ',output)))

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
