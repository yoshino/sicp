;---------------------------------------------------------------------
; 3.48: デッドロックの回避方法
;---------------------------------------------------------------------
; account1側からの処理: account1をロック=> account2をロック
; account2側からの処理account2をロック=> account1をロック

; 上記のような処理を同時に行うとデッドロックにハマる。
; 上記のような原因はロックの書ける方向に決まりが無いからである。

; account_idを導入し以下のように修正した。

(define (make-account-and-serializer balance account_id) ; 作成時にaccount_id
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        " Insufficient funds "))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance )

  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw ) withdraw)
            ((eq? m 'deposit ) deposit)
            ((eq? m 'balance ) balance)
            ((eq? m 'account_id ) account_id)           ; account_idを返すインターフェース
            ((eq? m 'serializer ) balance-serializer)
            (else (error " Unknown request : MAKE-ACCOUNT " m))))
    dispatch ))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'require)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p))

  (define (make-mutex)
    (let ((cell (list #f)))
      (define (the-mutex m)
        (cond ((eq? m 'acquire)
               (if (test-and-set! cell)
                   (the-mutex 'acquire))) ; trueならブロックが解除されるまでループ処理
              ((eq? m 'release) (clear! cell))))
      the-mutex))

  (define (clear! cell) (set-car! cell #f))

  (define (test-and-set! cell)
    (if (car cell)
        #t
        (begin (set-car! cell #t) #f))))

; インターフェース
(define (serialized-exchange account1 account2)
  (if (account1 'account_id) < (account2 'account_id))
    (let ((serializer1 (account1 'serializer)) ; ロックする方向をaccount_idが低い順にした
          (serializer2 (account2 'serializer)))
      ((serializer1 (serializer2 exchange))
       account1
       account2 ))
    (let ((serializer2 (account2 'serializer))
          (serializer1 (account1 'serializer)))
      ((serializer2 (serializer1 exchange))
       account2
       account1)))

; 例えば以下のような２つの口座を交換する時を考える。
; accountA account_id:1
; accountB account_id:2

; account_idが小さい順にロックをかけていくので

; accountA側の処理
; accountA => accountB

; accountB側の処理
; accountA => accountB

; accountAをロックした後、すでにaccountBがロックされている状態というのは起こり得ない。

;---------------------------------------------------------------------
; 3.49: デッドロックの回避方法が効かないケース
;---------------------------------------------------------------------
; 上記の方法は３つの時には成り立たない。

; accountA account_id:1
; accountB account_id:2
; accountC account_id:3

; AとB  BとCで交換する場合を考える。

; AとB
; accuntA => accountB

; BとC
; accountB => accountC

; accountAをロックして、BとCの交換のためにaccountBをロックした場合、
; AとBの交換はデッドロックに入る。

; といっても、Cの処理が終わればAとBの処理は流れていくのでデッドロックではないか、、、

; 正解は以下の通りらしい。。。。
; ある口座の内容によって次にアクセスする口座の内容がかわるような状況


