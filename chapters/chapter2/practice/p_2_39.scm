(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
    result
    (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

; reverse
(define (reverse items)
  (reverse-iter items (list)))

(define (reverse-iter items result)
  (if (null? items)
    result
    (reverse-iter (cdr items) (cons (car items) result))))

;gosh> (reverse (list 1 2 3))
;(3 2 1)

; appendはリストとリストを結合する。
;gosh> (append (list 1 2 3) (list 4 5 6))
;(1 2 3 4 5 6)
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
; gosh> (append (list 2) (list 1))
; (2 1)
; gosh> (append (list 3) (list 2 1))
; (3 2 1)

; appendは要素と要素を結合してリストをつくる
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))
;gosh> (cons 1 ())
;(1)
;gosh> (cons 2 (list 1))
;(2 1)
;gosh> (cons 3 (list 2 1))
;(3 2 1)
