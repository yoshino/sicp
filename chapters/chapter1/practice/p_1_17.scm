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
(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (fast-multi a (halve b))))
        (else (+ a (fast-multi a (- b 1))))))

