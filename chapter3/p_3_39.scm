(define x 10)

; 実行順序が保証されない並列化処理
(parallel-execute
  (lambda () (set! x (* x x)))
  (lambda () (set! x (+ x 1))))

;直列化手続きはserializerによって作る
(define s (make-serializer))

(parallel-execute
  (s (lambda () (set! x (* x x))))
  (s (lambda () (set! x (+ x 1)))))

;------------------------------------------------------
; 3.39
;------------------------------------------------------
(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () set! x ((s lambda() (* x x))))   ; P1
  (s (lambda () (set! x (+ x 1)))))           ; P2

;処理はどの順序で実行されるか？

; P1が先に終わる時
10 * 10 = 100
100 + 1 = 101

; P2が先に終わる時
10 + 1 = 11
11 * 11 = 121

; P1が100を算出 => P2が11をセット => 100をセット
100

; 以下の２つは消える
11 ; P2の処理の実行が始まればP1はｘへアクセスできないので

110 ; x * x の計算途中に他のプロセスはxにアクセスできないので

;------------------------------------------------------
; 3.40
;------------------------------------------------------
(define x 10)

; どのような結果がありうるか？
(parallel-execute
  (lambda () (set! x (* x x)))
  (lambda () (set! x (* x x x))))

; ここの結果
10 * 10      = 100
10 * 10 * 10 = 1000

; xにアクセスする時にもう１つの処理が終わっているかどうかに関連するので

; x*x*xの方が速くset!される場合
10 * 10 * 10    = 1000

10       * 1000 = 10000
1000     * 1000 = 1000000

; x*xの方が速くsetされる場合
10 * 10 = 100

10  * 10  * 100    = 10000
10  * 100 * 100    = 100000
100 * 100 * 100    = 1000000

; 直列化処理を行った場合
(define x 10)
(define s (make-serializer))
(parallel-execute
  (s (lambda () (set! x (* x x))))
  (s (lambda () (set! x (* x x x)))))

10  * 10        = 100
100 * 100 * 100 = 1000000

10 * 10 * 10   = 1000
1000  * 1000   = 1000000
;------------------------------------------------------
; 3.41
;------------------------------------------------------
; Benの提案
(let ((protected (make-serializer)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) (protected withdraw))
          ((eq? m 'deposit ) (protected deposit ))
          ((eq? m 'balance ) ((protected (lambda () balance )))) ; 直列化
          (else ( error " Unknown request : MAKE-ACCOUNT "
                        m))))

; もとの実装
(let ((protected (make-serializer)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) (protected withdraw))
          ((eq? m 'deposit ) (protected deposit ))
          ((eq? m 'balance ) balance)
          (else (error " Unknown request : MAKE-ACCOUNT "
                       m))))

; Benの言っていることは、データへの参照にも直列化処理を追加するということだけど、
; 参照はデータに変更を加えないので必要ない

;---------------------------------------------------
; 3.42
;---------------------------------------------------
; Ben
; 口座が引き出し手続きを要求されるたびに、(口座作成と同時に作った) 同じ直列化手続きを返すことになる実装
(let ((protected (make-serializer)))
  (let ((protected-withdraw (protected withdraw))
        (protected-deposit  (protected deposit)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) protected-withdraw)
            ((eq? m 'deposit ) protected-deposit )
            ((eq? m 'balance ) balance )
            (else (error "Unknown request : MAKE-ACCOUNT" m))))

; 正しくない
; 直列化はその時のプロセスに対してロックするので、口座を作成した時点でのプロセスで直列化すると、
; そのプロセスが生き続けてしまい、たぶん引き出しも預かり入れもすることができない。

; 正解は安全な変更らしい。
; 直列化をあらじめ定義しておいて、プロセス自体はdispatch呼び出し度に更新されるらしい.



