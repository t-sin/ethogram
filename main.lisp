(defpackage :ethogram
  (:use :cl)
  (:export))
(in-package :ethogram)

(defstruct testcase
  name
  input expected actual succeeded-p)

(defun valid-context-p (subject)
  (when (or (functionp subject) (symbolp subject))
    (values nil (format nil "not a function: ~s" subject)))
  t)

(defun parse-defs (sexp)
  (let ((defs (loop
                :for (k$for input k$returns expected) :on sexp by #'cddddr
                :unless (and (eq k$for :for) (eq k$returns :returns))
                :do (error "a mulformed test: ~s" (list k$for input k$returns expected))
                :collect (list :input input :expected expected))))
    (nreverse defs)))

(defun prepare-testcases (defs)
  (let ((testcases (make-array (length defs))))
    (loop
      :for n :from 0 :below (length defs)
      :for def := (elt defs n)
      :do (setf (elt testcases n)
                (make-testcase :input (getf def :input)
                               :expected (getf def :expected))))
    testcases))

(defun prepare (subject defexp)
  (multiple-value-bind (validp msg)
      (valid-context-p subject)
    (unless validp (error msg)))
  (let* ((testdefs (parse-defs defexp))
         (testcases (prepare-testcases testdefs)))
    ;; TODO: store tests
    testcases))

(defun run-testcases (subject testcases)
  (loop
    :for n :from 0 :below (length testcases)
    :for testcase := (elt testcases n)
    :for input := (let ((input (testcase-input testcase)))
                    (if (atom input)
                        (list input)
                        input))
    :for actual := (apply subject input)
    :do (setf (testcase-actual testcase) actual)
    :do (let ((expected (testcase-expected testcase)))
          (setf (testcase-succeeded-p testcase)
                (equalp actual expected)))))

(defun collect-result (testcases)
  (let ((failed ()))
    (loop
      :for n :from 0 :below (length testcases)
      :for testcase := (elt testcases n)
      :unless (testcase-succeeded-p testcase)
      :do (push testcase failed))
    (nreverse failed)))

(defun report-result (subject failed)
  (loop
    :for n :from 0 :below (length failed)
    :for testcase := (elt failed n)
    :do (format t "~a expected ~a but an actual ~a~%"
                subject
                (testcase-expected testcase)
                (testcase-actual testcase))))

(defun validate (subject testcases)
  (run-testcases subject testcases)
  (let ((failed (collect-result testcases)))
    (report-result subject failed)))

(defmacro test (context &body exp)
  (let ((subject (first exp))
        (defexp (rest exp)))
    (cond ((eq context :function)
           (prepare subject defexp))
    (t (error "a test context ~s is not implemented yet" context)))))
