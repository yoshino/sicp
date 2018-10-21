;---------------------------
; 両替の問題
;---------------------------

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denommination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denommination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;------------------------------
; コインが1種類の時を考える
;------------------------------
; (cc n 1)
; (cc n 0) (cc n-1 1)
; (cc n-1 0) (cc n-2 1)
; (cc n-2 0) (cc n-3 1)
; (cc n-3 0) (cc n-4 1)
; .
; .
; .
; (cc n-k 0) (cc n-k-1 1)

; 計算量: 2n+1
; 増加オーダー：O(n)

;------------------------------
; コインが2種類の時
;------------------------------
; (cc n 2)
; (cc n-5*1 2) (cc n 1)  ※  コインがい1種類の時をoneと統一して示す
; (cc n-5*2 2) one
; (cc n-5*3 2) one
; (cc n-5*4 2) one
; .
; .
; (cc n-5*k 2) one

; 例えば、n=50の時、k=10回、上の計算が実行される：n/5回である。

;　よって
; 計算量;　(n / 5) * (2*n + 1)
; 増加オーダー：O(n**2)

; 以上より増加オーダーはコインの種類が増えるに連れて指数関数的に増加する。

; n=3  O(n**3)
; n=4  O(n**4)
; n=5  O(n**5)
