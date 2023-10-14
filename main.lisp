(defpackage :ethogram
  (:use :cl)
  (:export))
(in-package :ethogram)

(defparameter *catalogues* ())

(defstruct catalogue
  name subject cases)

(defstruct pattern
  input expected actual succeeded-p)

(defun valid-context-p (subject)
  (when (or (functionp subject) (symbolp subject))
    (values nil (format nil "not a function: ~s" subject)))
  t)

(defun generate-name (subject)
  (if (symbolp subject)
      (format nil "function ~A" (symbol-name subject))
      (format nil "function ~A" subject)))

(defun parse-defs (sexp)
  (let ((defs (loop
                :for (k$for input k$returns expected) :on sexp by #'cddddr
                :unless (and (eq k$for :for) (eq k$returns :returns))
                :do (error "a mulformed test: ~s" (list k$for input k$returns expected))
                :collect (list :input input :expected expected))))
    (nreverse defs)))

(defun prepare-patterns (defs)
  (let ((patterns (make-array (length defs))))
    (loop
      :for n :from 0 :below (length defs)
      :for def := (elt defs n)
      :do (setf (elt patterns n)
                (make-pattern :input (getf def :input)
                              :expected (getf def :expected))))
    patterns))

(defun prepare (subject defexp)
  (multiple-value-bind (validp msg)
      (valid-context-p subject)
    (unless validp (error msg)))
  (let* ((name (generate-name subject))
         (testdefs (parse-defs defexp))
         (patterns (prepare-patterns testdefs))
         (test (make-catalogue :name name
                               :subject subject
                               :cases patterns)))
    (push test *catalogues*)
    test))

(defmacro test (context &body exp)
  (let ((subject (first exp))
        (defexp (rest exp)))
    (cond ((eq context :function)
           (let (($subject (gensym)))
             `(let ((,$subject ,subject))
                ;; when will the `prepare` be run at compile-time or runtime?
                ;; I feel it's run at runtime maybe...
                (prepare ,$subject ',defexp))))
          (t (error "a test context ~s is not implemented yet" context)))))

(defun run-patterns (subject patterns)
  (loop
    :for n :from 0 :below (length patterns)
    :for pattern := (elt patterns n)
    :for input := (let ((input (pattern-input pattern)))
                    (if (atom input)
                        (list input)
                        input))
    :for actual := (apply subject input)
    :do (setf (pattern-actual pattern) actual)
    :do (let ((expected (pattern-expected pattern)))
          (setf (pattern-succeeded-p pattern)
                (equalp actual expected)))))

(defun collect-result (patterns)
  (let ((failed ()))
    (loop
      :for n :from 0 :below (length patterns)
      :for pattern := (elt patterns n)
      :unless (pattern-succeeded-p pattern)
      :do (push pattern failed))
    (nreverse failed)))

(defun report-result (subject failed)
  (loop
    :for n :from 0 :below (length failed)
    :for pattern := (elt failed n)
    :do (format t "~a expected ~a but an actual ~a~%"
                subject
                (pattern-expected pattern)
                (pattern-actual pattern))))

(defun validate (test &key report)
  (let ((subject (catalogue-subject test))
        (patterns (catalogue-cases test)))
    (run-patterns subject patterns)
    (let ((failed (collect-result patterns)))
      (if report
          (report-result subject failed)
          failed))))

(defun validate-all (&key report)
  (let ((failed ()))
    (loop
      :for test :in *catalogues*
      :do (setf failed (nconc failed (validate test))))
    (if report
        (loop
          :for pattern :in failed
          :do (report-result "<test name here>" failed))
        failed)))
