(defpackage :ethogram
  (:use :cl)
  (:export))
(in-package :ethogram)

(defstruct testcase
  input expected actual result)

(defmacro test (context &body dsl)
  (cond ((eq context :function)
         (let ((subject (first dsl))
               (rest (rest dsl)))
           (when (or (functionp subject) (symbolp subject))
             (error "not a function: ~s" subject))
           (let* ((testdefs (loop
                              :for test := (subseq rest 0 4)
                              :for (k$for input k$returns expected) :on (rest dsl) by #'cddddr
                              :unless (and (eq k$for :for) (eq k$returns :returns))
                              :do (error "a mulformed test: ~s" test)
                              :collect (list :input input :expected expected)))
                  (num-of-defs (length testdefs)))
             (setf testdefs (nreverse testdefs))
             (alexandria:with-gensyms (%subject %testdefs %testdef %testcases
                                                %n %testcase %input %actual %failed)
               `(let ((,%subject ,subject)
                      (,%testdefs ',testdefs)
                      (,%testcases (make-array ,num-of-defs))
                      (,%failed ()))
                  ;; initializing
                  (loop
                    :for ,%n :from 0 :below ,num-of-defs
                    :for ,%testdef := (nth ,%n ,%testdefs)
                    :do (setf (aref ,%testcases ,%n)
                              (make-testcase :input (getf ,%testdef :input)
                                             :expected (getf ,%testdef :expected))))
                  ;; running tests
                  (loop
                    :for ,%n :from 0 :below ,num-of-defs
                    :for ,%testcase := (aref ,%testcases ,%n)
                    :for ,%input := (let ((,%input (testcase-input ,%testcase)))
                                      (if (atom ,%input)
                                          (list ,%input)
                                          ,%input))
                    :for ,%actual := (apply ,%subject ,%input)
                    :do (setf (testcase-actual ,%testcase)
                              ,%actual)
                    :do (setf (testcase-result ,%testcase)
                              (equalp ,%actual (testcase-expected ,%testcase))))
                  ;; collect results
                  (loop
                    :for ,%n :from 0 :below ,num-of-defs
                    :for ,%testcase := (aref ,%testcases ,%n)
                    :unless (testcase-result ,%testcase)
                    :do (push ,%testcase ,%failed))
                  ,%testcases)))))
        (t (error "a test context ~s is not implemented yet" context))))
