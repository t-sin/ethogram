(defpackage :ethogram.test.context.function
  (:use :cl))
(in-package :ethogram.test.context.function)

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
  (format t "~%running a test: ~a... ~%~%" desc)
  (handler-bind ((error #'test.report-failure))
    (funcall fn)))

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
   (test.not-equal (ethogram:all-catalogues) ())))
