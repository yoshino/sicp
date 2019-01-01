(use util.stream)

; 係数のベース
(define ones (stream-cons 1 ones))

(define (integrate-series s)
  (stream-map format s))

(define (format s)
  (* s (/ 1 2)))

(define s12 (integrate-series ones))
