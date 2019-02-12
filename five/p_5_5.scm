;----------------------------------------------------------
; factorialの再帰: 再帰階乗マシン
;----------------------------------------------------------
; スタック：後入れ先出し
; save:入れ　restore:出し
; val: 答え

(controller
  (assign continue (label fact-done))
;set up final return address
fact-loop
  (test (op =) (reg n) (const 1)) ;
  (branch (label base-case))
  ;; Set up for the recursive call by saving n and continue.
  ;; Set up continue so that the computation will continue
  ;; at after-fact when the subroutine returns.
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label fact-loop))
after-fact
  (restore n)
  (restore continue)
  (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
  (goto (reg continue))
;return to caller
base-case
  (assign val (const 1))
  ;base case: 1! = 1
  (goto (reg continue))
  ;return to caller
fact-done)

; 例えば(factorial 5)の時、スタックは以下のようにつまれる
n: 2
c: after-fact
n: 3
c: after-fact
n: 4
c: after-fact
n: 5
c: fact-done

; n=1, c=after-factの時にbase-case
val = 1

; after-factで積まれたスタックをrestoreしていく
; after-fact
n->2
c->after-fact
val = 1 * 2
; after-fact
n->3
c->after-fact
val = 1 * 2 * 3
; after-fact
n->4
c->after-fact
val = 1 * 2 * 3 * 4
; after-fact
n->5
c->fact-done
val = 1 * 2 * 3 * 4 * 5
;fact-done

;----------------------------------------------------------
; fibの再帰: フィボナッチマシン
;----------------------------------------------------------
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(controller
  (assign continue (label fib-done))
fib-loop
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))
  ;; Fib(n − 1) を求める準備
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n) ; n の古い値を保存
  (assign n (op -) (reg n) (const 1)) ; n を n-1 で上書き
  (goto (label fib-loop)) ; 再帰呼び出しの実行
afterfib-n-1
  ; リターン時に Fib(n − 1) は val に入っている
  (restore n)
  (restore continue)
  ;; Fib(n − 2) を求める準備
  (assign n (op -) (reg n) (const 2))
  (save continue)
  (assign continue (label afterfib-n-2))
  (save val)
  ; Fib(n − 1) を保存
  (goto (label fib-loop))
afterfib-n-2
  ; リターン時に Fib(n − 2) は val に入っている
  (assign n (reg val))
  ; n には Fib(n − 2) が入る
  (restore val)
  ; val には Fib(n − 1) が入る
  (restore continue)
  (assign val (op +) (reg val) (reg n)) ; Fib(n − 1) + Fib(n − 2)
  (goto (reg continue )) ; 呼び出し元に戻る、答えはval の中
immediate-answer
  (assign val (reg n))
  ; 基底の場合: Fib(n) = n
  (goto (reg continue ))
fib-done)

;fib-loop
;ex: (fib 5)
;------------------------------
*Stack*
n: 2
c: afterfib-n-1
n: 3
c: afterfib-n-1
n: 4
c: afterfib-n-1
n: 5
c: fib-done

n=1
c=afterfib-n-1

;immediate-answer
;------------------------------
val=1

;afterfib-n-1
;------------------------------
;restore
n=2
;restore
c=afterfib-n-1

n=0

;save c: afterfib-n-1

c=afterfib-n-2

;save v: 1

*Stack*
v: 1
c: afterfib-n-1
n: 3
c: afterfib-n-1
n: 4
c: afterfib-n-1
n: 5
c: fib-done

;fib-loop
;------------------------------
; n < 2 なので

;immediate-answer
;------------------------------
; n=0, c=afterfib-n-2
val = 0

;afterfib-n-2
;------------------------------
n=0
;restore
val=1
;restore
c =afterfib-n-1
val= 1 + 0

*Stack*
n: 3
c: afterfib-n-1
n: 4
c: afterfib-n-1
n: 5

;afterfib-n-1
;------------------------------
;restore
n=3
;restore
c=afterfib-n-1
n=1 ; 3-2
;save: c: afterfib-n-1
c=afterfib-n-2
;save: v: (1 + 0)

*Stack*
v: (1 + 0)
c: afterfib-n-1
n: 4
c: afterfib-n-1
n: 5
c: fib-done

;afterfib-n-2
;------------------------------
n= (1 + 0)
;restore
v= (1 + 0)
;restore
c=afterfib-n-1
v= (0 + 1 + 1) ; n=1

*Stack*
n: 4
c: afterfib-n-1
n: 5
c: fib-done

;afterfib-n-1
;------------------------------
n=2
c: afterfib-n-2

*Stack*
v: 2
c: afterfib-n-1
n: 5
c: fib-done

;afterfib-n-2
;------------------------------
n = 2
;restore
v = 2
;restore
c=afterfib-n-1

v=4

*Stack*
n: 5
c: fib-done

;afterfib-n-1
;------------------------------
;restore
n=5
;restore
c=fib-done
n=3
;save: fib-done
c=afterfib-n-2
;save: val 4

*Stack*
v: 4
c: fib-done

;afterfib-n-2
;------------------------------
n=3
;restore
v=4
;restore
c=fib-done
v
