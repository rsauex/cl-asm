(uiop:define-package #:cl-asm/src/asm-base
  (:use #:cl)
  (:import-from #:flexi-streams
                #:octet
                #:with-output-to-sequence)
  (:export #:instruction-size
           #:instruction-encode
           #:write-word
           #:assemble))

(in-package #:cl-asm/src/asm-base)

(defconstant +max-error-number+ 10)

(defgeneric instruction-size (instruction &rest arguments))
(defgeneric instruction-encode (instruction output symbol-table current-addr &rest arguments))

(defun first-pass (code)
  "Traverse the code, collect labels, compute intructions offsets."
  (let ((offset 0)                      ; Offset in bytes
        (symbol-table (make-hash-table))
        (error-number 0))
    (block loop
      (dolist (entry code)
        (progn;; handler-case
            (etypecase entry
              ;; label
              (symbol
               (setf (gethash entry symbol-table) offset))
              ;; instruction
              (list
               (incf offset (apply #'instruction-size entry))))
          ;; (error (e)
          ;;   (format *error-output* "ERROR on ~A:~%~A~%" entry e)
          ;;   (incf error-number)
          ;;   (when (> error-number +max-error-number+)
          ;;     (return-from loop)))
            )))
    (if (zerop error-number)
        symbol-table
        (format *error-output* "~A error occured during 1st pass!~%" error-number))))

(defun write-word (word stream)
  (write-byte (ldb (byte 8 0) word) stream)
  (write-byte (ldb (byte 8 8) word) stream))

(defun second-pass (code output symbol-table)
  (let ((current-addr 0))
   (dolist (entry code)
     (etypecase entry
       ;; label
       (symbol
        nil)
       ;; instruction
       (list
        (incf current-addr (apply #'instruction-size entry))
        (apply #'instruction-encode (first entry) output symbol-table current-addr (rest entry)))))))

(defun assemble (code)
  "This function generates machine instructions for assembler code."
  ;; First pass
  (let ((symbol-table (first-pass code)))
    ;; Second pass
    (with-output-to-sequence (output :element-type 'octet)
      (second-pass code output symbol-table))))
