(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))

; 上記の定義のものと環境構造はどのように異なるだろうか？
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

;gosh> (W1 50)
;50
;gosh> (W2 50)
;50

;-----------------------
; E1

; initial-amount: 100
;----------------------

; W1によって規定されている：場所の形成は変わらない
;-----------------------
; E2

; balance: 200
;----------------------



