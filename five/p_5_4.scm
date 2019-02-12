;------------------------------------------------------------
; p:5.4
;------------------------------------------------------------
;a. 再帰的指数計算
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

controller
  (assign continue (label expt-done))  ;set up final return address
  (assign res (const 1))               ;set up final initial value
expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  ; 処理
  (assign res (op multi) (reg res) (const 5))
  (assign n (op sun) (reg n) (const 1))
  (goto (label expt-loop))
base-case
  (assign val (const 1))
  (assign res (op multi) (reg res) (reg val))
  (goto (reg continue))
expt-done


;b. 反復的指数計算
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))

controller
  (assign n (read n))
  (assign continue (label expt-done))  ;set up final return address
  (assign product  (const 1))
  (assign counter  (reg n))
expt-loop
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))
  ;処理
  (assign product  (op multi) (reg product) (reg n))
  (assign counter  (op sub) (reg counter) (const 1))
  (goto (label expt-loop))
expt-done

;----------------------------------------------------------
; めも
;----------------------------------------------------------
;----------------------------------------------------------
; 5.9:コントローラ命令列の重複を防ぐため continueレジスタを使う
;----------------------------------------------------------
gcd
  (test (op =) (reg b) ( const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd))
gcd-done
  (test (op =) (reg continue) (const 0))
  (branch (label after-gcd-1))
  (goto (label after-gcd-2)) ;goto文の後も実行される
  ;; 最初に必要になる場所から gcd に分岐する前に
  ;; レジスタ continue に 0 を置く
  (assign continue (const 0)) ;
  (goto (label gcd))
after-gcd-1
;; 二回目に gcd を使う前にはレジスタ continue に 1 を置く
  (assign continue (const 1)) ; １は使用中
  (goto (label gcd))
after-gcd-2

;----------------------------------------------------------
; 5.10:continue レジスタにラベルを代入すると図 5.9の戦略を単純化・一般化できる
;----------------------------------------------------------
; レジスタにラベルを代入するとはポインタ(番地)の概念にほかならない

gcd
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done ))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd))
gcd-done
  (goto (reg continue))
  ;; gcd を呼ぶ前に、gcd から帰る先のラベルを continue に代入する
  (assign continue (label after-gcd-1))
  (goto (label gcd))
after-gcd-1
  ;; 二つ目の gcd 呼び出し、別の継続を持つ
  (assign continue (label after-gcd-2))
  (goto (label gcd))
after-gcd-2

