(defpackage :ethogram.test
  (:use :cl
        :ethogram))
(in-package :ethogram.test)

(defun test.checked? ()
  (let ((test (defspec #'checked?)))
    (assert (null (checked? test)))
    (check test)
    (assert (not (null (checked? test))))))

(test.checked?)
