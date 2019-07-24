(uiop:define-package #:cl-asm/src/avr
  (:use #:cl #:cl-asm/src/asm-base))

;;; TODO:
;; - export

(eval-when (:compile-toplevel)
  (defun check-clause (clasue)
    (destructuring-bind (parameters . values) clasue
      (let ((names nil))
        (unless (= 0 (mod (length values) 16))
          (error "Number of values must be a multiple of 16"))
        (dolist (parameter parameters)
          (destructuring-bind (p-name p-type) parameter
            (unless (and (symbolp p-name)
                         (= 1 (length (symbol-name p-name))))
              (error "Parameter name must be a single letter symbol"))
            (unless (symbolp p-type)
              (error "Parameter type must be a symbol"))
            (when (member p-name names)
              (error "Parameter name must be unique"))
            (push p-name names)))
        (dolist (value values)
          (unless (or (equal value 1)
                      (equal value 0)
                      (member value names))
            (error "Value must be either 0 or 1 or a parameter name"))))))

  (defun make-proper-parameter-list (parameters values)
    (let ((var-bits-number (make-hash-table)))
      (dolist (value values)
        (when (symbolp value)
          (setf (gethash value var-bits-number)
                (1+ (or (gethash value var-bits-number) 0)))))
      (loop :for (p-name p-type) :in parameters
            :collect (list p-name p-type (gethash p-name var-bits-number)))))
  
  (defmacro define-instruction (name &body clauses)
    (when (stringp (first clauses))
      (pop clauses))
    (dolist (clause clauses)
      (check-clause clause))
    (labels ((%make-p-type (p-type p-bits-number)
               (if p-bits-number
                   `(,p-type ,p-bits-number)
                   p-type))
             (%make-matcher (parameters)
               `(and (= (length parameters) ,(length parameters))
                     ,@(loop :for (p-name p-type p-bits-number) :in parameters
                             :for i :from 0
                             :collect `(and (nth ,i parameters)
                                            (typep (nth ,i parameters)
                                                   ',(%make-p-type p-type p-bits-number))))))
             (%make-value (values)
               (let ((b-number (make-hash-table))
                     (new-values nil))
                 (loop :for value :in (reverse values)
                       :for i :from 0
                       :do (push (if (numberp value)
                                     (ash value i)
                                     `(ash (ldb (byte 1 ,(setf (gethash value b-number)
                                                               (1+ (or (gethash value b-number) -1))))
                                                ,value)
                                           ,i))
                                 new-values))
                 `(logior
                   ,@new-values))))
      `(progn
         (defmethod instruction-size ((name (eql ',name)) &rest parameters)
           (cond
             ,@(loop :for (parameters . values) :in clauses
                     :collect (let ((parameters (make-proper-parameter-list parameters values)))
                                `(,(%make-matcher parameters)
                                  ,(/ (length values) 8))))))
         (defmethod instruction-encode ((name (eql ',name)) output symbol-table &rest parameters)
           (cond
             ,@(loop :for (parameters . values) :in clauses
                     :collect (let ((parameters (make-proper-parameter-list parameters values)))
                                `(,(%make-matcher parameters)
                                  (let (,@(loop :for (p-name p-type p-bits-number) :in parameters
                                                :for i :from 0
                                                :collect `(,p-name (encode-param-by-type (nth ,i parameters)
                                                                                         ',p-type ,p-bits-number))))
                                    (write-word ,(%make-value values) output)))))))))))

(defgeneric encode-param-by-type (parameter type bits-number))

;; NOTE: 10 bits - is a hack for instruction-aliases with one register duplicating
(deftype register (bits-number)
  (ecase bits-number
    ((5 10)
     '(member
       r0  r1  r2  r3  r4  r5  r6  r7
       r8  r9  r10 r11 r12 r13 r14 r15
       r16 r17 r18 r19 r20 r21 r22 r23
       r24 r25 r26 r27 r28 r29 r30 r31))
    (4
     '(member
       r16 r17 r18 r19 r20 r21 r22 r23
       r24 r25 r26 r27 r28 r29 r30 r31))
    (3
     '(member
       r16 r17 r18 r19 r20 r21 r22 r23))))

(defmethod encode-param-by-type (parameter (type (eql 'register)) (bits-number (eql 10)))
  (let ((for-5 (encode-param-by-type parameter 'register 5)))
    (logior (ldb (byte 4 0) for-5)
            (ash (ldb (byte 4 0) for-5) 4)
            (ash (ldb (byte 1 4) for-5) 8)
            (ash (ldb (byte 1 4) for-5) 9))))

(defmethod encode-param-by-type (parameter (type (eql 'register)) (bits-number (eql 5)))
  (ecase parameter
    (r0   0) (r1   1) (r2   2) (r3   3) (r4   4) (r5   5) (r6   6) (r7   7)
    (r8   8) (r9   9) (r10 10) (r11 11) (r12 12) (r13 13) (r14 14) (r15 15)
    (r16 16) (r17 17) (r18 18) (r19 19) (r20 20) (r21 21) (r22 22) (r23 23)
    (r24 24) (r25 25) (r26 26) (r27 27) (r28 28) (r29 29) (r30 30) (r31 31)))

(defmethod encode-param-by-type (parameter (type (eql 'register)) (bits-number (eql 4)))
  (ecase parameter
    (r16  0) (r17  1) (r18  2) (r19  3) (r20  4) (r21  5) (r22  6) (r23  7)
    (r24  8) (r25  9) (r26 10) (r27 11) (r28 12) (r29 13) (r30 14) (r31 15)))

(defmethod encode-param-by-type (parameter (type (eql 'register)) (bits-number (eql 3)))
  (ecase parameter
    (r16  0) (r17  1) (r18  2) (r19  3) (r20  4) (r21  5) (r22  6) (r23  7)))

(deftype register-pair (bits-number)
  (ecase bits-number
    (4 '(member
         r0-1   r2-3   r4-5   r6-7
         r8-9   r10-11 r12-13 r14-15
         r16-19 r18-19 r20-21 r22-23
         r24-25 r26-27 r28-29 r30-39))
    (2 '(member
         r24-25 r26-27 r28-29 r30-31))))

(defmethod encode-param-by-type (parameter (type (eql 'register-pair)) (bits-number (eql 4)))
  (ecase parameter
    (r0-1    0) (r2-3    1) (r4-5    2) (r6-7    3)
    (r8-9    4) (r10-11  5) (r12-13  6) (r14-15  7)
    (r16-19  8) (r18-19  9) (r20-21 10) (r22-23 11)
    (r24-25 12) (r26-27 13) (r28-29 14) (r30-39 15)))

(defmethod encode-param-by-type (parameter (type (eql 'register-pair)) (bits-number (eql 2)))
  (ecase parameter
    (r24-25 1) (r26-27 2) (r28-29 3) (r30-39 4)))

(deftype immediate (bits-number)
  `(unsigned-byte ,bits-number))

(defmethod encode-param-by-type (parameter (type (eql 'immediate)) bits-number)
  parameter)

(deftype io-port (bits-number)
  `(unsigned-byte ,bits-number))

(defmethod encode-param-by-type (parameter (type (eql 'io-port)) bits-number)
  parameter)

(deftype code-offset (bits-number)
  (declare (ignore bits-number))
  'symbol)

(defmethod encode-param-by-type (parameter (type (eql 'code-offset)) bits-number)
  (error "Not implemented yet!"))

(deftype code-address (bits-number)
  (declare (ignore bits-number))
  'symbol)

(defmethod encode-param-by-type (parameter (type (eql 'code-address)) bits-number)
  (error "Not implemented yet!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Arithmetic

(define-instruction add
  "Add"
  (((d register) (r register))
   0 0 0 0  1 1 r d  d d d d  r r r r))

(define-instruction adc
  "Add with carry"
  (((d register) (r register))
   0 0 0 1  1 1 r d  d d d d  r r r r))

(define-instruction adiw
  "Add immediate to word"
  (((d register-pair) (k immediate))
   1 0 0 1  0 1 1 0  k k d d  k k k k))


(define-instruction sub
  "Substract"
  (((d register) (r register))
   0 0 0 1  1 0 r d  d d d d  r r r r))

(define-instruction subc
  "Substract with carry"
  (((d register) (r register))
   0 0 0 0  1 0 r d  d d d d  r r r r))

(define-instruction subi
  "Substract immediate"
  (((d register) (k immediate))
   0 1 0 1  k k k k  d d d d  k k k k))

(define-instruction sbci
  "Substract immediate with carry"
  (((d register) (k immediate))
   0 1 0 0  k k k k  d d d d  k k k k))

(define-instruction sbiw
  "Substract immediate from word"
  (((d register-pair) (k immediate))
   1 0 0 1  0 1 1 1  k k d d  k k k k))


(define-instruction mul
  "Multiply unsigned"
  (((d register) (r register))
   1 0 0 1  1 1 r d  d d d d  r r r r))

(define-instruction muls
  "Multiply signed"
  (((d register) (r register))
   0 0 0 0  0 0 1 0  d d d d  r r r r))

(define-instruction mulsu
  "Multiply signed with unsigned"
  (((d register) (r register))
   0 0 0 0  0 0 1 1  0 d d d  0 r r r))


(define-instruction fmul
  "Fractional multiply unsigned"
  (((d register) (r register))
   0 0 0 0  0 0 1 1  0 d d d  1 r r r))

(define-instruction fmuls
  "Fractional multiply signed"
  (((d register) (r register))
   0 0 0 0  0 0 1 1  1 d d d  0 r r r))

(define-instruction fmulsu
  "Fractional multiply signed with unsigned"
  (((d register) (r register))
   0 0 0 0  0 0 1 1  1 d d d  1 r r r))


(define-instruction dec
  "Decrement"
  (((d register))
   1 0 0 1  0 1 0 d  d d d d  1 0 1 0))

(define-instruction inc
  "Increment"
  (((d register))
   1 0 0 1  0 1 0 d  d d d d  0 0 1 1))

;;; Logic

(define-instruction and
  "Logical AND"
  (((d register) (r register))
   0 0 1 0  0 0 r d  d d d d  r r r r))

(define-instruction andi
  "Logical AND with immediate"
  (((d register) (k immediate))
   0 1 1 1  k k k k  d d d d  k k k k))

(define-instruction or
  "Logical OR"
  (((d register) (r register))
   0 0 1 0  1 0 r d  d d d d  r r r r))

(define-instruction ori
  "Logical OR with immediate"
  (((d register) (k immediate))
   0 1 1 0  k k k k  d d d d  k k k k))

(define-instruction eor
  "Logical XOR"
  (((d register) (r register))
   0 0 1 0  0 1 r d  d d d d  r r r r))


(define-instruction com
  "One's complement"
  (((d register))
   1 0 0 1  0 1 0 d  d d d d  0 0 0 0))

(define-instruction neg
  "Two's complement"
  (((d register))
   1 0 0 1  0 1 0 d  d d d d  0 0 0 1))


(define-instruction lsl
  "Logical shift left (ADD Rd,Rd)"
  (((d register))
   0 0 0 0  1 1 d d  d d d d  d d d d))

(define-instruction lsr
  "Logical shift right"
  (((d register))
   1 0 0 1 0 1 0 d  d d d d  0 1 1 0))

(define-instruction asr
  "Arithmetic shift right"
  (((d register))
   1 0 0 1  0 1 0 d  d d d d  0 1 0 1))


(define-instruction rol
  "Rotate left though Carry (ADC Rd,Rd)"
  (((d register))
   0 0 0 1  1 1 d d  d d d d  d d d d))

(define-instruction ror
  "Rotate right though Carry"
  (((d register))
   1 0 0 1  0 1 0 d  d d d d  0 1 1 1))


(define-instruction clr
  "Clear register (EOR Rd,Rd)"
  (((d register))
   0 0 1 0  0 1 d d  d d d d  d d d d))

(define-instruction ser
  "Set all bits in register"
  (((d register))
   1 1 1 0  1 1 1 1  d d d d  1 1 1 1))


(define-instruction sbr
  "Set bits in register"
  (((d register) (k immediate))
   0 1 1 0  k k k k  d d d d  k k k k))

;; TODO: implement
;; (define-instruction cbr
;;   "Clear bits in register (ANDI with K complemented)"
;;   )


(define-instruction swap
  "Swap nibbles"
  (((d register))
   1 0 0 1  0 1 0 d  d d d d  0 0 1 0))

;;; Branches

(define-instruction sbic
  "Skip if bit in I/O register is unset"
  (((a io-port) (b immediate))
   1 0 0 1  1 0 0 1  a a a a  a b b b))

(define-instruction sbis
  "Skip if bit in I/O register is set"
  (((a io-port) (b immediate))
   1 0 0 1  1 0 1 1  a a a a  a b b b))


(define-instruction sbrc
  "Skip if bit in register is unset"
  (((r register) (b immediate))
   1 1 1 1  1 1 0 r  r r r r  0 b b b))

(define-instruction sbrs
  "Skip if bit in register is set"
  (((r register) (b immediate))
   1 1 1 1  1 1 1 r  r r r r  0 b b b))


(define-instruction brbc
  "Brunch if bit in SREG is unset"
  (((s immediate) (k code-offset))
   1 1 1 1  0 1 k k  k k k k  k s s s))

(define-instruction brbs
  "Brunch if bit in SREG is set"
  (((s immediate) (k code-offset))
   1 1 1 1  0 0 k k  k k k k  k s s s))

(define-instruction brcc
  "Brunch if Carry bit is unset"
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 0 0 0))

(define-instruction brcs
  "Brunch if Carry bit is set"
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 0 0 0))

(define-instruction brsh
  "Brunch if >= (unsigned)"
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 0 0 0))

(define-instruction brlo
  "Brunch if < (unsigned)"
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 0 0 0))

(define-instruction brne
  "Brunch if /="
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 0 0 1))

(define-instruction breq
  "Brunch if =="
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 0 0 1))

(define-instruction brpl
  "Brunch if Plus"
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 0 1 0))

(define-instruction brmi
  "Brunch if Minus"
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 0 1 0))

(define-instruction brvc
  "Brunch if Overflow bit is unset"
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 0 1 1))

(define-instruction brvs
  "Brunch if Overflow bit is set"
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 0 1 1))

(define-instruction brge
  "Brunch if >= (signed)"
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 1 0 0))

(define-instruction brlt
  "Brunch if < (signed)"
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 1 0 0))

(define-instruction brhc
  "Brunch if Half Carry bit is unset"
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 1 0 1))

(define-instruction brhs
  "Brunch if Half Carry bit is set"
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 1 0 1))

(define-instruction brtc
  "Brunch if T bit is unset"
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 1 1 0))

(define-instruction brts
  "Brunch if T bit is set"
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 1 1 0))

(define-instruction brid
  "Brunch if Global Interrupt is disabled"
  (((k code-offset))
   1 1 1 1  0 1 k k  k k k k  k 1 1 1))

(define-instruction brie
  "Brunch if Global Interrupt is enabled"
  (((k code-offset))
   1 1 1 1  0 0 k k  k k k k  k 1 1 1))

;;; Bits

(define-instruction cbi
  "Unset bit in I/O register"
  (((a io-port) (b immediate))
   1 0 0 1  1 0 0 0  a a a a  a b b b))

(define-instruction sbi
  "Set bit in I/O register"
  (((a io-port) (b immediate))
   1 0 0 1  1 0 1 0  a a a a  a b b b))


(define-instruction bld
  "Set T bit to register bit"
  (((d register) (b immediate))
   1 1 1 1  1 0 0 d  d d d d  0 b b b))

(define-instruction bst
  "Set T bit from register bit"
  (((d register) (b immediate))
   1 1 1 1  1 0 1 d  d d d d  0 b b b))


(define-instruction bset
  "Set bit in SREG"
  (((s immediate))
   1 0 0 1  0 1 0 0  0 s s s  1 0 0 0))

(define-instruction bclr
  "Clean bit in SREG"
  (((s immediate))
   1 0 0 1  0 1 0 0  1 s s s  1 0 0 0))


(define-instruction clc
  "Clear Carry bit"
  (()
   1 0 0 1  0 1 0 0  1 0 0 0  1 0 0 0))

(define-instruction clz
  "Clear Zero bit"
  (()
   1 0 0 1  0 1 0 0  1 0 0 1  1 0 0 0))

(define-instruction cln
  "Clear Negative bit"
  (()
   1 0 0 1  0 1 0 0  1 0 1 0  1 0 0 0))

(define-instruction clv
  "Clear Overflow bit"
  (()
   1 0 0 1  0 1 0 0  1 0 1 1  1 0 0 0))

(define-instruction cls
  "Clear Signed bit"
  (()
   1 0 0 1  0 1 0 0  1 1 0 0  1 0 0 0))

(define-instruction clh
  "Clear Half Carry bit"
  (()
   1 0 0 1  0 1 0 0  1 1 0 1  1 0 0 0))

(define-instruction clt
  "Clear T bit"
  (()
   1 0 0 1  0 1 0 0  1 1 1 0  1 0 0 0))

(define-instruction cli
  "Clear Global Interrupt bit"
  (()
   1 0 0 1  0 1 0 0  1 1 1 1  1 0 0 0))


(define-instruction sec
  "Set Carry bit"
  (()
   1 0 0 1  0 1 0 0  0 0 0 0  1 0 0 0))

(define-instruction sez
  "Set Zero bit"
  (()
   1 0 0 1  0 1 0 0  0 0 0 1  1 0 0 0))

(define-instruction sen
  "Set Negative bit"
  (()
   1 0 0 1  0 1 0 0  0 0 1 0  1 0 0 0))

(define-instruction sev
  "Set Overflow bit"
  (()
   1 0 0 1  0 1 0 0  0 0 1 1  1 0 0 0))

(define-instruction ses
  "Set Signed bit"
  (()
   1 0 0 1  0 1 0 0  0 1 0 0  1 0 0 0))

(define-instruction seh
  "Set Half carry bit"
  (()
   1 0 0 1  0 1 0 0  0 1 0 1  1 0 0 0))

(define-instruction set
  "Set T bit"
  (()
   1 0 0 1  0 1 0 0  0 1 1 0  1 0 0 0))

(define-instruction sei
  "Set Global interrupt bit"
  (()
   1 0 0 1  0 1 0 0  0 1 1 1  1 0 0 0))

;;; Compare

(define-instruction cp
  "Compare"
  (((d register) (r register))
   0 0 0 1  0 1 r d  d d d d  r r r r))

(define-instruction cpc
  "Compare with carry"
  (((d register) (r register))
   0 0 0 0  0 1 r d  d d d d  r r r r))

(define-instruction cpi
  "Compare with immediate"
  (((d register) (k immediate))
   0 0 1 1  k k k k  d d d d  k k k k))

(define-instruction cpse
  "Compare & Skip if equal"
  (((d register) (r register))
   0 0 0 1  0 0 r d  d d d d  r r r r))


(define-instruction tst
  "Test for Zero or Minus (AND Rd, Rd)"
  (((d register))
   0 0 1 0  0 0 d d  d d d d  d d d d))

;;; Call & jump

(define-instruction call
  "Call to a procedure"
  (((k code-address))
   1 0 0 1  0 1 0 k  k k k k  1 1 1 k
   k k k k  k k k k  k k k k  k k k k))

(define-instruction rcall
  "Relative call to a procedure"
  (((k code-offset))
   1 1 0 1  k k k k  k k k k  k k k k))

(define-instruction icall
  "Indirect call to a procedure"
  (()
   1 0 0 1  0 1 0 1  0 0 0 0  1 0 0 1))

(define-instruction eicall
  "Extended indirect call to a procedure"
  (()
   1 0 0 1  0 1 0 1  0 0 0 1  1 0 0 1))


(define-instruction ret
  "Return from a procedure"
  (()
   1 0 0 1  0 1 0 1  0 0 0 0  1 0 0 0))

(define-instruction iret
  "Return from a interrupt"
  (()
   1 0 0 1  0 1 0 1  0 0 0 1  1 0 0 0))


(define-instruction jmp
  "Jump"
  (((k code-address))
   1 0 0 1  0 1 0 k  k k k k  1 1 0 k
   k k k k  k k k k  k k k k  k k k k))

(define-instruction rjmp
  "Relative jump"
  (((k code-offset))
   1 1 0 0  k k k k  k k k k  k k k k))

(define-instruction ijmp
  "Indirect jump"
  (()
   1 0 0 1  0 1 0 0  0 0 0 0  1 0 0 1))

(define-instruction eijmp
  "Extended indirect jump"
  (()
   1 0 0 1  0 1 0 0  0 0 0 1  1 0 0 1))

;;; I/O

(define-instruction in
  "Read from a I/O register"
  (((d register) (a io-port))
   1 0 1 1  0 a a d  d d d d  a a a a))

(define-instruction out
  "Write to a I/O register"
  (((a io-port) (r register))
   1 0 1 1  1 a a r  r r r r  a a a a))

;;; Memory

;; LAC   'Z'     r : R   1001 001r rrrr 0110   ;; - Load and clear
;; LAS   'Z'     r : R   1001 001r rrrr 0101   ;; - Load and set
;; LAT   'Z'     r : R   1001 001r rrrr 0111   ;; - Load and toggle

;; LD    d : R   'X'     1001 000d dddd 1100   ;; - Load indirect
;; LD    d : R   'X+'    1001 000d dddd 1101   ;; - Load indirect
;; LD    d : R   '-X'    1001 000d dddd 1110   ;; - Load indirect

;; LD    d : R   'Y'     1000 000d dddd 1000   ;; - Load indirect
;; LD    d : R   'Y+'    1001 000d dddd 1001   ;; - Load indirect
;; LD    d : R   '-Y'    1001 000d dddd 1010   ;; - Load indirect
;; LDD   d : R   'Y+'q:I 10q0 qq0d dddd 1qqq   ;; - Load indirect

;; LD    d : R   'Z'     1000 000d dddd 0000   ;; - Load indirect
;; LD    d : R   'Z+'    1001 000d dddd 0001   ;; - Load indirect
;; LD    d : R   '-Z'    1001 000d dddd 0010   ;; - Load indirect
;; LDD   d : R   'Z+'q:I 10q0 qq0d dddd 0qqq   ;; - Load indirect


(define-instruction ldi
  "Load immediate"
  (((d register) (k immediate))
   1 1 1 0  k k k k  d d d d  k k k k))


(define-instruction lds
  "Load direct from Data space"
  (((d register) (k immediate))
   1 0 0 1  0 0 0 d  d d d d  0 0 0 0
   k k k k  k k k k  k k k k  k k k k)
  ;; (((d register) (k immediate))
  ;;  1 0 1 0  0 k k k  d d d d  k k k k)
  )

(define-instruction lpm
  "Load indirect from Program memory"
  (()
   1 0 0 1  0 1 0 1  1 1 0 0  1 0 0 0))
;; LPM   d : R   'Z'     1001 000d dddd 0100   ;; - Load indirect from Program memory
;; LPM   d : R   'Z+'    1001 000d dddd 0101   ;; - Load indirect from Program memory

(define-instruction elpm
  "Extended load program memory"
  (()
   1 0 0 1  0 1 0 1  1 1 0 1  1 0 0 0))
;; ELPM  d : R   'Z'     1001 000d dddd 0110   ;; - Extended load program memory
;; ELPM  d : R   'Z+'    1001 000d dddd 0111   ;; - Extended load program memory

(define-instruction mov
  "Copy register"
  (((d register) (r register))
   0 0 1 0  1 1 r d  d d d d  r r r r))

(define-instruction movw
  "Copy register word"
  (((d register-pair) (r register-pair))
   0 0 0 0  0 0 0 1  d d d d  r r r r))


(define-instruction pop
  "Pop register from stack"
  (((d register))
   1 0 0 1  0 0 0 d  d d d d  1 1 1 1))

(define-instruction push
  "Push register to stack"
  (((r register))
   1 0 0 1  0 0 1 r  r r r r  1 1 1 1))


(define-instruction spm
  "Store program memory"
  (()
   1 0 0 1  0 1 0 1  1 1 1 0  1 0 0 0))
;; SPM   'Z+'            1001 0101 1111 1000   ;; - Store program memory

;; ST    'X'     r : R   1001 001r rrrr 1100   ;; - Store indirect
;; ST    'X+'    r : R   1001 001r rrrr 1101   ;; - Store indirect
;; ST    '-X'    r : R   1001 001r rrrr 1110   ;; - Store indirect
;; ST    'Y'     r : R   1000 001r rrrr 1000   ;; - Store indirect
;; ST    'Y+'    r : R   1001 001r rrrr 1001   ;; - Store indirect
;; ST    '-Y'    r : R   1001 001r rrrr 1010   ;; - Store indirect
;; STD   'Y+'q:I r : R   10q0 qq1r rrrr 1qqq   ;; - Store indirect
;; ST    'Z'     r : R   1000 001r rrrr 0000   ;; - Store indirect
;; ST    'Z+'    r : R   1001 001r rrrr 0001   ;; - Store indirect
;; ST    '-Z'    r : R   1001 001r rrrr 0010   ;; - Store indirect
;; STD   'Z+'q:I r : R   10q0 qq1r rrrr 0qqq   ;; - Store indirect

(define-instruction sts
  "Store direct to Data space"
  (((k immediate) (r register))
   1 0 0 1  0 0 1 r  r r r r  0 0 0 0
   k k k k  k k k k  k k k k  k k k k)
  ;; (((k immediate) (r register))
  ;;  1 0 1 0  1 k k k  d d d d  k k k k)
  )


(define-instruction xch
  "Exchange register and memory"
  (((d register))
   1 0 0 1  0 0 1 d  d d d d  0 1 0 0))

;;; Misc

(define-instruction wdr
  "Watchdog reset"
  (()
   1 0 0 1  0 1 0 1  1 0 1 0  1 0 0 0))

(define-instruction break
  "Hardware break"
  (()
   1 0 0 1  0 1 0 1  1 0 0 1  1 0 0 0))

(define-instruction sleep
  "Sleep mode"
  (()
   1 0 0 1  0 1 0 1  1 0 0 0  1 0 0 0))

(define-instruction nop
  "No operation"
  (()
   0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
