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

; パスワードを導入する
(define (make-account balance password)
  (define (correct-password? input)
    (if (eq? input password)
      #t
      (error "Incorrect password")))
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-pass m)
    (if (correct-password? input-pass)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unkown request: MAKE-ACCOUNT"
                         m)))))
  dispatch)

(define acc (make-account 100 'pass))

;gosh> ((acc 'pass 'withdraw) 40)
;60
;gosh> ((acc 'wrong-pass 'withdraw) 40)
;*** ERROR: Incorrect password
