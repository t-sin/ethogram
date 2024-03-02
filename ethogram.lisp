(defpackage :ethogram
  (:use :cl)
  (:export
   ;; behavior
   :malformed-behavior-error
   :reason
   :parse-behavior
   :behavior
   :behavior-desc
   :behavior-subject
   :behavior-check
   ;; example
   :malformed-examples-error
   :parse-function-examples
   :examples
   :function-examples
   :function-examples-input
   :function-examples-output
   ;; checking
   :check
   ;; introspection
   :all-catalogs
   :clear-catalogs))
(in-package :ethogram)

(defparameter *catalogs* (make-hash-table))
(defun all-catalogs () *catalogs*)
(defun clear-catalogs ()
  (setf *catalogs* (make-hash-table)))

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
            :collect `(let ((,$input (list ,@(loop
                                               :for elem :in input
                                               :collect elem)))
                            (,$output ,(if (and (listp output)
                                                (symbolp (first output))
                                                (string= (symbol-name (first output)) "VALUES"))
                                           `(multiple-value-list ,output)
                                           `'(,output))))
                        (make-function-examples
                         :input ,$input
                         :output ,$output)))))))

(define-condition malformed-behavior-error (error)
  ((reason :initform nil
           :initarg :reason)))

(defun parse-behavior (body)
  (when (null body)
    (signal (make-condition 'malformed-behavior-error
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
      (signal (make-condition 'malformed-behavior-error
                              :reason ":SUBJECT is required")))
    (values subject prepare dispose examples)))

(defstruct behavior
  desc
  subject
  prepare
  dispose
  examples
  check)

(defmacro behavior (desc &body body)
  (multiple-value-bind (subject prepare dispose examples)
      (parse-behavior body)
    (alexandria:with-gensyms ($subject $examples $input $behavior)
      `(let* ((,$subject ,subject)
              (,$examples ,examples)
              (,$behavior (make-behavior :desc ,desc
                                   :subject ,$subject
                                   :check (lambda (,$input)
                                            (apply ,$subject ,$input))
                                   ,@(when prepare
                                       `(:prepare (lambda () ,prepare)))
                                   ,@(when dispose
                                       `(:dispose (lambda () ,dispose)))
                                   :examples ,$examples)))
         (alexandria:nconcf (gethash ,$subject *catalogs*) (list ,$behavior))
         ,$behavior))))

(defgeneric prepare (behavior))
(defmethod prepare ((behavior behavior))
  (unless (null (behavior-prepare behavior))
    (funcall (behavior-prepare behavior))))

(defgeneric dispose (behavior))
(defmethod dispose ((behavior behavior))
  (unless (null (behavior-dispose behavior))
    (funcall (behavior-dispose behavior))))

(defgeneric check (behavior))
(defmethod check ((behavior behavior))
  (prepare behavior)
  (loop
    :for example :in (behavior-examples behavior)
    :for input := (function-examples-input example)
    :for expected := (function-examples-output example)
    :for actual := nil
    :for result := nil
    :do (unwind-protect
             (flet ((do-nothing (c)
                      (declare (ignorable c))
                      (format t "~%SIGNALED WHILE CHECKING:~%  ~s~%  IN BEHAVIOR ~s~%"
                              c (behavior-desc behavior))
                      (return-from check)))
               (handler-bind ((condition #'do-nothing))
                 (setf actual (multiple-value-list (funcall (behavior-check behavior) input)))
                 (setf result (equal actual expected))))
          (dispose behavior)
          (format t "a spec ~s is ~a~%"
                  (behavior-desc behavior)
                  (if result
                      "succeeded"
                      "failed")))
    :collect result))
