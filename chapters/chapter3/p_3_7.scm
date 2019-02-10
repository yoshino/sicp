;関数型言語と両立しない代入という概念

; 代入を使用している
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))

;gosh> (W 20)
;5
;gosh> (W 10)
;-5

; 代入をしない
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-decrementer 25))

;内部状態を維持できていない
;gosh> (D 20)
;5
;gosh> (D 10)
;15

; 代入の本質は名前＝値の関連性を破壊するということ。
; 代入を導入すると名前は値ではなく値を収める場所に変わる。
; 参照透過性がやぶられた。

; 参照透過性が破られた世界ではその人の中身をみても、その人の本当のことはわからない。
; その人の本質は内部ではなく、その人のいる場所によって規定される。
; このことは、絶対的価値観の放棄なのだろうか。
; たぶん、そうではないだろう。

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))

;gosh> (factorial 5)
;120

; これを代入を利用して書く。
(define (factorial n)
  (let ((product 1)
        (counter 1))
  (define (iter)
    (if (> counter n)
      product
      (begin (set! product (* counter product)) ; ここの順番を変更すると計算があわない
             (set! counter (+ counter 1))
      (iter))))
  (iter)))

;-------------------------------------------------------
; 3.7: 口座の結合
;-------------------------------------------------------

; パスワードを導入した銀行口座
(define (make-account balance password)
  (let ((passwords (list password))) ; 複数のパスワードを登録できるようにした

    (define (correct-password? input-pass)
      (define (inclide-pass? input-pass submitted-passwords)
        (cond ((null? submitted-passwords)
               (begin (error "Incorrect Password!")
                      #f))
              ((if (eq? (car submitted-passwords) input-pass)
                 #t
                 (inclide-pass? input-pass (cdr submitted-passwords))))))
      (inclide-pass? input-pass passwords))

    (define (add-password new-password)
      (set! passwords (cons new-password passwords)))

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
              ((eq? m 'add-password) add-password)
              (else (error "Unkown request: MAKE-ACCOUNT"
                           m)))))
    dispatch))

(define peter-acc (make-account 100 'peter-pass))

;gosh> ((peter-acc 'peter-pass 'withdraw) 10)
;70
;gosh> ((peter-acc 'wrong-pass 'withdraw) 10)
;*** ERROR: Incorrect Password!

;gosh> ((peter-acc 'peter-pass 'add-password) 'paul-pass)
;(paul-pass peter-pass)
;gosh> ((peter-acc 'paul-pass 'withdraw) 10)
;90
;gosh> ((peter-acc 'peter-pass 'withdraw) 10)
;80

; この口座に新しいパスワードを利用してアクセスできるようにする
(define (make-joint account pass new-pass)
  ((account pass 'add-password) new-pass)
  account)

(define paul-acc (make-joint peter-acc 'peter-pass 'paul-pass))

;gosh> ((paul-acc 'paul-pass 'withdraw) 10)
;90
;gosh> ((peter-acc 'peter-pass 'withdraw) 10)
;80
