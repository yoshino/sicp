(use util.stream)

; quotientは商
(define (expand num den radix)
  (stream-cons
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define (show-head-stream s n)
     (define (iter s n)
       (if (= n 0)
           'done
           (begin
             (display (stream-car s))
             (newline)
             (show-head-stream (stream-cdr s) (- n 1)))))
     (iter s n))

(define s1 (expand 1 7 10))
;gosh> (show-head-stream s1 10)
;1
;4
;2
;8
;5
;7
;1
;4
;2
;8

; 10/7: 1.428571428....

(define s2 (expand 3 8 10))
;gosh> (show-head-stream s2 10)
;3
;7
;5
;0
;0
;0
;0
;0
;0
;0

; 30/8 = 3.75000000...
