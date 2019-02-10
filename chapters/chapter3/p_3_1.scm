(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

; 上記のようにすとbalanceがグローバルにアクセスできてしまうので変更

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

; 異なるオブジェクトを作成することができる
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 200))

;gosh> (W1 10)
;90
;gosh> (W2 10)
;190

; 預かり入れも引き出しも実装する
; メッセージパッシングを適用している
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unkown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))

;gosh> ((acc 'withdraw) 50)
;50
;gosh> ((acc 'withdraw) 60)
;"Insufficient funds"
;gosh> ((acc 'dposit) 40)
;*** ERROR: Unkown request: MAKE-ACCOUNT dposit
;gosh> ((acc 'deposit) 40)
;90

;---------------------------------------
; 3.1
;---------------------------------------
; アキュムレーター
(define (make-accumulator start_value)
  (let ((sum start_value))
    (lambda (v)
      (begin (set! sum (+ sum v))
              sum))))

(define A (make-accumulator 5))

;gosh> (A 10)
;15
;gosh> (A 10)
;25
;gosh> (A 10)
;35
