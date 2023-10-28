(defpackage :ethogram.test.context.function
  (:use :cl))
(in-package :ethogram.test.context.function)

(defparameter *tests* ())

(defun $test.add (test-fn)
  (push test-fn *tests*))

(defun test.run-all ()
  (format t "running all tests about a testing context: function...~%")
  (loop
    :for test :in (reverse *tests*)
    :do (funcall test))
  (format t "finished all tests about a testing context: function!~%~%"))

(defun test.equal (actual expected)
  (unless (equal actual expected)
    (error "these are not equal: expected ~s but actual ~s"
           expected actual)))

(defun test.not-equal (actual expected)
  (when (equal actual expected)
    (error "these are equal: expected ~s but actual ~s"
           expected actual)))

(defun test.report-failure (e)
  (format t "test failed!~%~%reason: ~a" e))

(defun test.behavior (desc fn)
  (let ((test (lambda ()
                (format t "  running a test: ~a... ~%" desc)
                (handler-bind ((error #'test.report-failure))
                  (funcall fn)))))
    ($test.add test)))

(test.behavior
 "Successfully define a test for and it's stored in *catalogues "
 (lambda ()
   ;; ensure internally stored catalogeus are empty
   (ethogram:clear-catalogues)
   (test.equal (ethogram:all-catalogues) ())
   ;; define a catalogue for function
   (ethogram:test :function #'oddp
     :for 1 :returns t)
   ;; check catalogues is not empty
   (test.not-equal (ethogram:all-catalogues) ())
   ;; check a test about #'oddp is defined correctly
   (let ((target (ethogram:all-catalogues)))
     (test.equal (length target) 1))
   (let ((target (elt (ethogram:all-catalogues) 0)))
     (test.equal (type-of target) 'ethogram::catalogue))))
