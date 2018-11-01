; 再帰
(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
      (/ (n i) (+ (d i) (frac (+ i 1))))
      (/ (n i) (d i))))
  (frac 1))

;gosh> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)
;0.625
;gosh> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
;0.6179775280898876
;gosh> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
;0.6180339887498948
;gosh> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1000)
;0.6180339887498948


; 反復
(define (cont-frac-iter n d k)
  (define (frac-iter i result)
    (if (= i 0)
      result
      (frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (frac-iter (- k 1) (/ (n k) (d k))))
