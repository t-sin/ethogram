(defpackage :ethogram.test
  (:use :cl
        :ethogram))
(in-package :ethogram.test)

;; # DSL example
;;
;; ;; 引数は&bodyにしてエディタのインデントを減らす
;; (defmacro examples (type &body body))
;;
;; (defspec "a function to check number's oddness"
;;   :subject #'oddp
;;
;;   (examples :function
;;     :returns t :for 1)
;;
;;   (examples :function
;;     :about "first argument is an odd number"
;;     :returns nil :for 0
;;     :returns nil :for 2
;;     :returns nil :for 10)
;;
;;   (examples :group
;;     :about "first argument is not an odd number"
;;     (examples :function
;;       :about "non-zero numbers"
;;       :returns t
;;       :for 1
;;       :for 3
;;       :for 1001)
;;     (examples :function
;;       :about "a zero"
;;       :returns t
;;       :for 0)))

;; # TODO
;;
;; - [x] 検査を実行する
;; - [x] 検査の前に:prepareを実行する
;; - [x] 検査の後に:disposeを実行する
;; - [x] 検査がコンディションを投げても:disposeを実行する
;; - [ ] 検査のタイトルをspec-descで取得できる
;; - [ ] `(examples :function ...)`で定義した内容で検査を実行できる
;; - [ ] 検査の結果をstdioに即時出力する
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
