;------------------------
; 遅い掛け算
;------------------------
(define (slow* a b)
  (if (= b 0)
    0
    (+ a (slow* a (- b 1)))))

;------------------------
; 再帰プロセス
;------------------------
;(define (double a)
;  (+ a a))

;(define (halve a)
;  (/ a 2))

;(define (even? n)
;  (= (remainder n 2) 0))

;(define (fast-multi a b)
;  (cond ((= b 0) 0)
;        ((= b 1) a)
;        ((even? b) (double (fast-multi a (halve b))))
;        (else (+ a (fast-multi a (- b 1))))))

;------------------------
; 再帰プロセス
;------------------------
(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-multi a b)
  (multi-iter a b 0))

(define (multi-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (multi-iter (double a) (halve b) p))
        (else (multi-iter a (- b 1) (+ a p)))))

;ex
; (fast-multi 2 5)
; (multi-iter 2 5 0)
; (multi-iter 2 4 2)
; (multi-iter 2**2 2 2)
; (multi-iter 2**3 1 2)
; (multi-iter 2**3 0 2 + 2**3)


