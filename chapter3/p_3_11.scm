(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount )
      (begin (set! balance (- balance amount))
             balance )
      "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount ))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw ) withdraw)
          ((eq? m 'deposit ) deposit)
          (else
            (error "Unknown request : MAKE-ACCOUNT"
                   m))))
  dispatch )

; 次の対話によって生成される環境構造を示せ。
(define acc (make-account 50))
((acc 'deposit ) 40)

; STEP1
(define acc (make-account 50))
;E1
; balance: 50

;globalにあるmake-accountを参照する

;E2
;withdraw
;dwposit
;dispath

; STEP2
((acc 'deposit ) 40)

;E3
;dispatchが呼び出される
;m: deposit

;E4
; deposit
; amount 40

; STEP3
(define acc2 (make-account 50))

;accと共有化している環境はGlobalに存在する
;make-acountくらい。

