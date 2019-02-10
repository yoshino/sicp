(use util.stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define integers2 (integers-starting-from 1))

(define (stream-head s n)
  (define counter 0)
  (define (iter s)
    (if (<= n counter)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (set! counter (+ counter 1))
        (iter (stream-cdr s)))))
  (iter s))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))       ; １番目: 一行目の始めのペア
    (interleave                                ; ２番目: ２行目のペア
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t))))) ; ３番目: １行目のそれ以降のペア

(define pairs-of-integers (pairs integers integers))

; 3.68
; 以下の様に単純化することは可能か？
(define (pairs-simple s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

(define pairs-of-integers-simple (pairs-simple integers integers))

;gosh> (stream-head pairs-of-integers-simple 10)
;(1 1)
;(2 2)
;(1 2)
;(2 3)
;(1 3)
;(3 3)
;(1 4)
;(2 4)
;(1 5)
;(3 4)

