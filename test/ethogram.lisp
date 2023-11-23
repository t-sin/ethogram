(defpackage :ethogram.test
  (:use :cl
        :ethogram))
(in-package :ethogram.test)

;; # DSL example
;;
;; (defspec #'oddp
;;   :to "Check number's oddness"
;;   (spec :about "first argument"
;;         (examples
;;          :when "the argument are odd numbers"
;;          :return nil :for 0
;;          :return nil :for 2
;;          :return nil :for 10)
;;         (examples
;;          :when "the argument are odd numbers"
;;          :expect t
;;          :for 1
;;          :for 3
;;          :for 1001)
;;         (examples
;;          :when "the argument is zero"
;;          :expect t :for 0)))

;; # TODO
;;
;; - [x] 検査を実行する
;; - [x] 検査の前に:prepareを実行する
;; - [x] 検査の後に:disposeを実行する
;; - [x] 検査がコンディションを投げても:disposeを実行する
;; - [ ] 複数の検査を実行する
;; - [ ] 検査の結果を収集する

(defun test.checked? ()
  (flet ((subject ()))
    (let ((test (defspec #'subject)))
      (assert (null (checked? test)))
      (check test)
      (assert (not (null (checked? test)))))))

(defun test.check-flow ()
  (let ((logs ()))
    (labels ((push-log (name) (push name logs))
             (prepare () (push-log :prepare))
             (dispose () (push-log :dispose))
             (subject ()
               (push-log :check)
               (signal "signal a condition")))
      (let ((test (defspec #'subject
                    :prepare #'prepare
                    :dispose #'dispose)))
        (check test)
        (assert (equal '(:prepare :check :dispose) (reverse logs)))))))

(test.checked?)
(test.check-flow)
