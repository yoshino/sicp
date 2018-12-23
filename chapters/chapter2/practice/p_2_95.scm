(define p1 ( make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 ( make-polynomial 'x '((2 11) (0 7))))
(define p3 ( make-polynomial 'x '((1 13) (0 5))))

; 多項式に整数部分が入っているのでうまくいかない
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
(define q3 (greatest-common-divisor q1 q2))

(define ( gcd-terms a b)
  (if ( empty-termlist? b)
    a
    (gcd-terms b ( remainder-terms a b))))


