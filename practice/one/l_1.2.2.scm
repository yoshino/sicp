;---------------------------
;木の再帰：フィボナッチ
;---------------------------

; 再帰
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
; 反復
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

; 以下のように展開する

; (fib 4)
; (fib-iter 1 0 4)
; (fib-iter 1 1 3)
; (fib-iter 2 1 2)
; (fib-iter 3 2 1)
; (fib-iter 5 3 0)

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

