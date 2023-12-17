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
   :malformed-examples-error
   :parse-function-examples
   :examples
   :function-examples
   :function-examples-input
   :function-examples-output
   ;; methods
   :check))
(in-package :ethogram)

(defstruct function-examples
  input output)

(define-condition malformed-examples-error (error)
  ((reason :initform nil
           :initarg :reason)))

(defun parse-function-examples (body)
  (when (null body)
    (signal (make-condition 'malformed-examples-error
                            :reason "empty")))
  (let (inputs outputs)
    (loop
      :named parse-loop
      :for form := (first body)
      :do (case form
            (:returns
             (push (second body) outputs)
             (setf body (cddr body)))
            (:for
             (push (second body) inputs)
             (setf body (cddr body)))
            (t (return-from parse-loop))))
    (when (zerop (length inputs))
      (signal (make-condition 'malformed-examples-error
                              :reason "there is no :FOR ARGLIST")))
    (when (zerop (length outputs))
      (signal (make-condition 'malformed-examples-error
                              :reason "there is no :RETURNS VALUES")))
    (unless (= (length inputs) (length outputs))
      (signal (make-condition 'malformed-examples-error
                              :reason "input/output must be a same length")))
    (loop
      :for i :in (nreverse inputs)
      :for o :in (nreverse outputs)
      :collect (list i o))))

(defmacro examples (type &body body)
  (assert (eq type :function))
  (let ((pairs (parse-function-examples body)))
    (alexandria:with-gensyms ($input $output)
      `(list
        ,@(loop
            :for (input output) :in pairs
            :collect `(let ((,$input '(,input))  ;; applyできるようにリストにいれている
                            (,$output ',output))
                        (make-function-examples
                         :input ,$input
                         :output ,$output)))))))

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
    (alexandria:with-gensyms ($subject $examples $input)
      `(let ((,$subject ,subject)
             (,$examples ,examples))
         (make-spec :desc ,desc
                    :subject ,$subject
                    :check (lambda (,$input)
                             (apply ,$subject ,$input))
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
  (loop
    :for example :in (spec-examples spec)
    :for input := (function-examples-input example)
    :for expected := (function-examples-output example)
    :for actual := nil
    :for result := nil
    :do (unwind-protect
             (flet ((do-nothing (c)
                      (declare (ignorable c))
                      (format t "~s" c)
                      (return-from check)))
               (handler-bind ((condition #'do-nothing))
                 (setf actual (funcall (spec-check spec) input))
                 (setf result (equal actual expected))))
          (dispose spec)
          (format t "a spec ~s is ~a~%"
                  (spec-desc spec)
                  (if result
                      "succeeded"
                      "failed")))
    :collect result))
