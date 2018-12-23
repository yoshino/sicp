;---------------------------------------------------------------------------
; 間違ったパスワードに連続して７回アクセスすると警察に通報するメソッドを追加
;---------------------------------------------------------------------------
(define (make-account balance password)
  (define pass-couner 0)
  (define (correct-password? input)
    (if (eq? input password)
      (begin (set! pass-couner 0)
             #t)
      (if (= pass-couner 2) ; 3回失敗すると警察を呼ぶ
        (call-the-cops)
        (begin (set! pass-couner (+ pass-couner 1))
               (error "Incorrect password")))))
  (define (call-the-cops)
    (error "Call the cops!!!!!!"))
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

;gosh> ((acc 'pass 'withdraw) 10)
;90
;gosh> ((acc 'wrong-pass 'withdraw) 10)
;*** ERROR: Incorrect password
;gosh> ((acc 'wrong-pass 'withdraw) 10)
;*** ERROR: Incorrect password
;gosh> ((acc 'wrong-pass 'withdraw) 10)
;*** ERROR: Call the cops!!!!!!
