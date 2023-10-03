(defpackage :ethogram
  (:use :cl)
  (:export :<result>
           :result-succeeded?
           :result-expected
           :result-actual

           :<context>
           :context-description
           :context-result))
(in-package :ethogram)

(defclass <result> ()
  ((succeeded? :initarg :succeeded?
              :initform nil
              :reader result-succeeded?)
   (expected :initarg :expected
             :initform nil
             :reader result-expected)
   (actual :initarg :actual
           :initform nil
           :reader result-actual)))

(defgeneric diff (result))

(defclass <context> ()
  ((description :initarg :subject
                :initform nil
                :type string
                :reader context-description)
   (result :initarg :result
           :initform ()
           :reader context-result)))

(defgeneric parse (context definition))
(defgeneric validate (context))

(defun dispatch-context (context-name))

(defclass <group-context> (<context>))
(defclass <test-context> (<context>))
(defgeneric examine (context))

(defmacro test ())

(defclass <function> (<context>)
  ((subject :type function))
  ((inputs :initarg :inputs
           :initform ()
           :accessor funtion-inputs)))
