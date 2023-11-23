(defpackage :ethogram.test
  (:use :cl
        :ethogram))
(in-package :ethogram.test)

;; # DSL example
;;
;; ;; 引数は&bodyにしてエディタのインデントを減らす
;; (defmacro fragment (type &body body))
;;
;; (defspec #'oddp
;;   :describe "Check number's oddness"
;;
;;   (fragment :function
;;     :returns t :for 1)
;;
;;   (fragment :examples
;;     :about "first argument is an odd number"
;;     (fragment :function
;;       "returns false"
;;       :returns nil :for 0
;;       :returns nil :for 2
;;       :returns nil :for 10))
;;
;;   (fragment :examples
;;     :about "first argument is not an  odd number"
;;     (fragment :function
;;       "returns true"
;;       :returns t
;;       :for 1
;;       :for 3
;;       :for 1001)))

;; # TODO
;;
;; - [x] 検査を実行する
;; - [x] 検査の前に:prepareを実行する
;; - [x] 検査の後に:disposeを実行する
;; - [x] 検査がコンディションを投げても:disposeを実行する
;; - [x] 検査したい内容を`:describe STRING or SYMBOL`で記述できる
;; - [x] 検査の名前を検査対象と検査したい内容 (:describe) から生成する
;; - [ ] 関数に対する検査内容を`:returns EXPECTED :for INPUT`で指定する
;; - [ ] 検査内容に従って検査する
;; - [ ] 検査の結果を出力する
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

(defun test.spec-name-without-description ()
  (let ((spec (defspec #'oddp))
        (expected "ODDP: a function"))
    (assert (typep (spec-name spec) 'string))
    (assert (string= (spec-name spec) expected))))

(defun test.spec-name ()
  (let* ((spec (defspec #'oddp :describe "a function to check a number's oddness"))
         (expected "ODDP: a function to check a number's oddness"))
    (assert (typep (spec-name spec) 'string))
    (assert (string= (spec-name spec) expected))))

(test.checked?)
(test.check-flow)
(test.spec-name-without-description)
(test.spec-name)
