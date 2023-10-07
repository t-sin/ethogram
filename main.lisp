(defpackage :ethogram
  (:use :cl)
  (:export))
(in-package :ethogram)

(defmacro test (context &body dsl)
  (cond ((eq context :function)
         (let ((subject (first dsl))
               (rest (rest dsl)))
           (when (or (functionp subject) (symbolp subject))
             (error "not a function: ~s" subject))
           (alexandria:with-gensyms (%subject)
             `(let ((,%subject ,subject))
                ,@(loop
                    :for (k$for input k$returns actual) :on rest by #'cddddr
                    :collect (let ((args (if (atom input)
                                             (list input)
                                             input)))
                               `(equalp (apply ,%subject ',args) ,actual)))))))
        (t (error "a test context ~s is not implemented yet" context))))
