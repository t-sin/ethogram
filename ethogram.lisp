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
  check-fn
  examples
  (checked? nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-spec (body)
    (print (list :spec-body body))
    (let (subject prepare dispose examples)
      (loop
        :for form := (first body)
        :do (typecase form
              (keyword (let ((value (second body)))
                         (ecase form
                           (:subject (setf subject value))
                           (:prepare (setf prepare value))
                           (:dispose (setf dispose value)))
                         (setf body (cddr body))))
              (cons (let ((name (car form)))
                      (assert (string= (symbol-name name) "EXAMPLES"))
                      (setf examples form)
                      (setf body (cdr body))))
              (null (return-from parse-spec (values subject prepare dispose examples)))))))


  (defun handle-checking-condition (c)
    (declare (ignorable c))
    (return-from handle-checking-condition))

  (defmacro defspec (desc &body body)
    (multiple-value-bind (subject prepare dispose examples)
        (parse-spec body)

      (print (list :result subject prepare dispose examples))
      (assert (not (null subject)))
      ;(assert (not (null examples)))
      (let (($spec (gensym))
            ($subject (gensym))
            ($examples (gensym))
            ($input (gensym))
            ($actual (gensym)))
        `(let* ((,$subject ,subject)
                (,$examples ,examples)
                (,$spec (make-spec :desc ,desc
                                   :examples ,$examples)))
           (setf (spec-check-fn ,$spec)
                 (lambda ()
                   (setf (spec-checked? ,$spec) nil)
                   ,prepare
                   (unwind-protect
                        (handler-bind ((condition #','handle-checking-condition))
                          (let* ((,$input (function-examples-input ,$examples))
                                 (,$actual (multiple-value-list (apply ,$subject ,$input))))
                            (equal ,$actual (function-examples-output ,$examples))))
                     (setf (spec-checked? ,$spec) t)
                     ,dispose)))
           ,$spec)))))

(defun checked? (spec)
  (spec-checked? spec))

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
  (funcall (spec-check-fn spec)))
