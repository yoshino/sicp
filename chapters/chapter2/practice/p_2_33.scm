; accumulate
; プログラムは抽象化を行うと、マップ、フィルタ、集積の組み合わせとして見ることができる。
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; map
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define l1 (list 1 2 3))
(define l2 (list 4 5 6 7 8))

; append
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

; length
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
