(use util.stream)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (scale-random-stream stream)
  (stream-map (lambda (x)
                 (* x (random-integer 100)))
                  stream))

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

(define ones (stream-cons 1 ones))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

; 通常の微分
(define (integral integrand initial-value dt)
(define int
  (stream-cons initial-value
               (add-streams (scale-stream integrand dt)
                            int)))
int)

; 遅延引数を利用した場合
(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons
      initial-value
      (let ((integrand (force delayed-integrand))) ; 二項目から評価する
        (add-streams (scale-stream integrand dt) int))))
  int)

; 遅延評価を使わないと定義できない
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define s (solve (lambda (y) y)
                   1
                   0.001))

;-------------------------------------------------
; 3.77
;-------------------------------------------------
(define (integral integrand initial-value dt)
  (cons-stream
    initial-value
    (if (stream-null? integrand)
        the-empty-stream
        (integral (stream-cdr integrand)
                  (+ (* dt (stream-car integrand))
                     initial-value)
                  dt))))

(define (integral delayed-integrand initial-value dt)
  (stream-cons
    initial-value
    (if (stream-null? integrand)
        the-empty-stream
        (integral (stream-cdr (force delayed-integrand))
                  (+ (* dt (stream-car integrand))
                     initial-value)
                  dt))))

;-------------------------------------------------
; 3.78
;-------------------------------------------------
; ループしながら定義する例
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
(define s (solve (lambda (y) y) 1 0.001))


(define (integral integrand initial-value dt)
  (cons-stream
    initial-value
    (if (stream-null? integrand)
        the-empty-stream
        (integral (stream-cdr integrand)
                  (+ (* dt (stream-car integrand))
                     initial-value)
                  dt))))


; 正解はこんな感じにdelayの対象が項全体になるようにする
(define (solve-2nd a b dt y0 dy0)
       (define y (integral (delay dy) y0 dt))
       (define dy (integral (delay ddy) dy0 dt))
       (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
       y)

(define s2 (solve-2nd 2 3 0.01 10 (lambda (y) y)))

;-------------------------------------------------
; 3.79: 3.78を一般化する
;-------------------------------------------------
; 上記の例でいえば、ddyの
; (add-streams (scale-stream a  (acale-stream b の部分が一般化されていない
(define (solve-2nd f dy0 y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)
