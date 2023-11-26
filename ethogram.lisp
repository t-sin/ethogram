(defpackage :ethogram
  (:use :cl)
  (:export
   ;; spec
   :malformed-spec-error
   :reason
   :parse-spec
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

(define-condition malformed-spec-error (error)
  ((reason :initform nil
           :initarg :reason)))

(defun parse-spec (body)
  (when (null body)
    (signal (make-condition 'malformed-spec-error
                            :reason "empty")))
  (let (subject prepare dispose examples)
    (loop
      :for form := (first body)
      :until (or (null form) (null form))
      :do (etypecase form
            (keyword (let ((value (second body)))
                       (setf body (cddr body))
                       (case form
                         (:subject (setf subject value))
                         (:prepare (setf prepare value))
                         (:dispose (setf dispose value)))))
            (list (setf examples form)
                  (setf body (rest body)))))
    (unless subject
      (signal (make-condition 'malformed-spec-error
                              :reason ":SUBJECT is required")))
    (values subject prepare dispose examples)))

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
