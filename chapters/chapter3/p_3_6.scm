(use srfi-27)

(define (rand internal-value range)
  (define generate
    (begin (set! internal-value (+ internal-value (random-integer 10)))
           internal-value))
  (define reset
    (begin (set! internal-value 100)
           internal-value))
  (define (dispatch m)
    (cond ((eq? m 'generate) generate)
          ((eq? m 'reset) reset)))
  dispatch)

; generateとresetは先に評価されてしまい、
; 以降、generateを呼び出しても最初に評価した同じ値が返ってきてしまう
(define rand100 (rand 100 10))
;(rand100 'generate)

(define rand
  (let ((internal-value 0))
    (define (generate)
      (begin (set! internal-value (+ internal-value (random-integer 10)))
             internal-value))
    (define (reset new-value)
      (begin (set! internal-value new-value)
             internal-value))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)))
    dispatch))

;gosh> (rand 'generate)
;1
;gosh> (rand 'generate)
;10
;gosh> (rand 'generate)
;18
;gosh> ((rand 'reset) 100)
;100
;gosh> (rand 'generate)
;101
;gosh> (rand 'generate)
;110
;gosh> (rand 'generate)
;119
