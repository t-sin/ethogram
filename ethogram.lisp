(defpackage :ethogram
  (:use :cl)
  (:export :defspec
           :ethogram-checked?
           :ethogram-check))
(in-package :ethogram)

(defun defspec (name))
(defun ethogram-checked? (ethogram))
(defun ethogram-check (ethogram))
