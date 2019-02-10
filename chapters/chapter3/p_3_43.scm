; ２つの口座が関連している場合
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((acocunt1 'withdraw) difference)
    ((account2 'diposit) difference)))

;両方の口座のシリアライザを使ってexchange 手続き全体を直列化するというものがあります。そのためには、口座のシリアライザにアクセスする準備をします。
;シリアライザを露出させることで、銀行口座オブジェクトのモジュール性を意図的に破っていることに注意してください

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        " Insufficient funds "))

  (define ( deposit amount )
    (set! balance (+ balance amount))
    balance )

  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw ) withdraw)
            ((eq? m 'deposit ) deposit)
            ((eq? m 'balance ) balance)
            ((eq? m 'serializer ) balance-serializer)
            (else (error " Unknown request : MAKE-ACCOUNT " m))))
    dispatch ))

; シリアライザをエクスポートすることによって柔軟性が得られ、交換プログラムを直列化したものを実装できるようになります。単純に、両方の口座のシリアライザによって元の exchange 手続きを直列化するだけです。

; 確かにaccount1 account2のモジュール性も破壊されていることがわかる。
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2 )))

; 個別にシリアライザを入れないとイケないので少しめんどくさいけど。
(define (deposit account amount)
  (let ((s ( account 'serializer))
        (d ( account 'deposit)))
    ((s d) amount)))

; 参考までに今までの実装
(let ((protected (make-serializer)))
  (let ((protected-withdraw (protected withdraw))
        (protected-deposit (protected deposit)))
    (define ( dispatch m)
      (cond ((eq? m 'withdraw ) protected-withdraw)
            ((eq? m 'deposit ) protected-deposit )
            ((eq? m 'balance ) balance )
            (else
              (error " Unknown request : MAKE-ACCOUNT"
                     m))))
    dispatch ))

;---------------------------------------------------------------
; 3.43
;---------------------------------------------------------------
; 口座残高が以下のようにスタートする
;A :10 ;B :20 ;C :30

; 例えば以下の様なプロセスが並行で実行されたとする。
1 (serialized-exchange A B)
3 (serialized-exchange A C)
2 (serialized-exchange B C)
4 (serialized-exchange A B)

;1の処理の途中AとBの口座へはアクセスできない。
;実行順序は保証されていないので、どの口座がいくらかになるかは保証されないが、
; 10 20 30の組み合わせは保証される 
A: 20 B: 10 C: 30

;2
A: 20 B: 30 C: 10

;3
A: 10 B: 30 C: 20

;4
A: 30 B: 10 C: 20

; シリアライザーでモジュール性を破壊しない場合
A: 10 B: 20 C: 30

1 (serialized-exchange A B)
Aの口座を20に置き換えた時点で

2 (serialized-exchange B C)

が完了して、
B: 30 C: 20

1のプロセスの続きのBをAの10に置き換わり最終的な値は、

A: 20 B 10 C: 20

;---------------------------------------------------------------
; 3.44
;---------------------------------------------------------------
; ある口座から別の口座への振込を考える

; 口座にまたがるシリアライザーは不要であるという主張は正しいか?
; from-accountの値は十分に大きいと想定する
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;正しい
;A:100 B:100 C:100

A:10 => B <= C:30

この時のBの状態の推移はAの処理とCの処理を直列化しないので以下のような場合がありうる。

100 => 130 => 140
100 => 110 => 140

;実行順序が最終的な値を変えないので、口座間にまたがる直列化は不要である。

;---------------------------------------------------------------
; 3.45
;---------------------------------------------------------------
; 参考までに今までの実装
(let ((protected (make-serializer)))
  (let ((protected-withdraw (protected withdraw))
        (protected-deposit (protected deposit)))
    (define ( dispatch m)
      (cond ((eq? m 'withdraw ) protected-withdraw)
            ((eq? m 'deposit ) protected-deposit )
            ((eq? m 'balance ) balance )
            (else
              (error " Unknown request : MAKE-ACCOUNT"
                     m))))
    dispatch))

; new
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount )) balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount )) balance)

  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))

; balance-serializer を上記のように定義する場合、exchaneの時に、
; withdraw depositメソッドへアクセスすることができない。

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2 )))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((acocunt1 'withdraw) difference)
    ((account2 'diposit) difference)))

; withdraw、deposit 手続きの部分で serializer が入れ子になるので処理が終わらない。
