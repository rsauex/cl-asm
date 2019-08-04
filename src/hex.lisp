(uiop:define-package #:cl-asm/src/hex
  (:use #:cl)
  (:import-from #:flexi-streams
                #:octet)
  (:export #:to-hex))

(in-package #:cl-asm/src/hex)

(defun to-hex (seq)
  (with-output-to-string (hex)
    (format hex ":020000020000FC~%")
    (loop :for l := (length seq) :then (- l 32)
          :for i :from 0 :by 32
          :while (> l 0)
          :do (let ((sum 0))
                (labels ((%add-byte (byte)
                           (format hex "~2,'0x" byte)
                           (incf sum byte)))
                  (format hex ":")
                  (%add-byte (min l 32))
                  (%add-byte #x00)
                  (%add-byte i)
                  (%add-byte #x00)
                  (loop :for o :from 0 :below (min l 32)
                        :do (%add-byte (aref seq (+ i o)))))
                (format hex "~2,'0x" (logand (- sum) #xFF)))
              (format hex "~%"))
    (format hex ":00000001FF")))
