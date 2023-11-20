(defpackage :ethogram.test
  (:use :cl
        :ethogram))
(in-package :ethogram.test)

(defun test.checked? ()
  (let ((test (defspec #'ethogram-checked?)))
    (print (ethogram-checked? test))
    (ethogram-check test)
    (print (ethogram-checked? test))))

(test.checked?)
