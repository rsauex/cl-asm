(defsystem #:cl-asm
  :description "Simple assembler framework "
  :author "Yurii Hryhorenko <yuragrig@ukr.net>"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:cl-asm/src/all)
  ;; :in-order-to ((test-op (test-op #:cl-asm/tests)))
  )

;; (defsystem #:cl-asm/tests
;;   :description "Tests for cl-asm"
;;   :author "Yurii Hryhorenko <yuragrig@ukr.net>"
;;   :class :package-inferred-system
;;   :defsystem-depends-on (:asdf-package-system)
;;   :depends-on (#:cl-asm #:cl-asm/tests/all)
;;   :perform (test-op (o c) (symbol-call :cl-asm/tests/all :run-test-suite)))

(register-system-packages "cl-asm/src/all" '(:cl-asm))
