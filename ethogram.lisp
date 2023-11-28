(defpackage :ethogram
  (:use :cl)
  (:export
   ;; spec
   :malformed-spec-error
   :reason
   :parse-spec
   :defspec
   :spec-desc
   :spec-subject
   :spec-check
   ;; example
   :examples
   :function-examples
   :function-examples-input
   :function-examples-output
   ;; methods
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
    (alexandria:with-gensyms ($input $output)
      `(let ((,$input ',input)
             (,$output ',output))
         (make-function-examples
          :input (if (or (null ,$input) (listp ,$input))
                     ,$input
                     (list ,$input))
          :output (if (or (null ,$output) (listp ,$output))
                      ,$output
                      (list ,$output)))))))

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
  examples
  check)

(defmacro defspec (desc &body body)
  (multiple-value-bind (subject prepare dispose examples)
      (parse-spec body)
    (alexandria:with-gensyms ($subject $examples)
      `(let ((,$subject ,subject)
             (,$examples ,examples))
         (make-spec :desc ,desc
                :subject ,$subject
                :check (lambda ()
                         (apply ,$subject
                                (function-examples-input ,$examples)))
                ,@(when prepare
                    `(:prepare (lambda () ,prepare)))
                ,@(when dispose
                    `(:dispose (lambda () ,dispose)))
                :examples ,$examples)))))

(defgeneric prepare (spec))
(defmethod prepare ((spec spec))
  (unless (null (spec-prepare spec))
    (funcall (spec-prepare spec))))

(defgeneric dispose (spec))
(defmethod dispose ((spec spec))
  (unless (null (spec-dispose spec))
    (funcall (spec-dispose spec))))

(defgeneric check (spec))
(defmethod check ((spec spec))
  (prepare spec)
  (let (actual expected result)
    (unwind-protect
         (flet ((do-nothing (c)
                  (declare (ignorable c))
                  (format t "~a" c)
                  (return-from check)))
           (handler-bind ((condition #'do-nothing))
             (setf expected (function-examples-output (spec-examples spec)))
             (setf actual (multiple-value-list (funcall (spec-check spec))))
             (setf result (equal actual expected))))
      (dispose spec)
      result)))
