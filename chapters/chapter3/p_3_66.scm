(use util.stream)

; 無限のペア
(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s)  (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1))))) ; s1とs2を交互に入れ替えて結合する

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define p (pairs integers integers))

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

(define res (stream-head p 100))

; (1 99) 196番目
; (99 100) (100 100)は出現しなかった

; 左側だけ注目すると

; 1: 1 2 4 6 8 10....
; 2: 3 5 9 13....
; 3: 7 11
; 4:

;1: (1 1)
;2: (1 2)
;3: (2 2)
;4: (1 3)
;5: (2 3)
;6: (1 4)
;7: (3 3)
;8: (1 5)
;9: (2 4)
;10: (1 6)
;11:(3 4)
;12:(1 7)
;13:(2 5)
;14:(1 8)


;法則性はよくわからない、、、
